# app.R
library(shiny)
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(httr)
library(jsonlite)
library(yaml)
library(plotly)
library(shinycssloaders)
library(stringr)
library(DT)
library(htmltools)
library(rmarkdown)
library(webshot) # Add this for PDF export

# Load OpenAI API key
credentials <- yaml::read_yaml("credentials.yml")
openai_api_key <- credentials$openai

# Helper function to make OpenAI API calls
generate_sql <- function(question, db_schema) {
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(
      "Authorization" = paste("Bearer", openai_api_key),
      "Content-Type" = "application/json"
    ),
    body = list(
      model = "gpt-4o-mini",
      messages = list(
        list(
          role = "system",
          content = paste0(
            "You are a SQL expert. Given a database schema and a question, ",
            "generate a SQL query to answer the question. Schema: ", db_schema
          )
        ),
        list(
          role = "user",
          content = question
        )
      )
    ),
    encode = "json"
  )
  
  if ((response$status_code) != 200) {
    stop("Error calling OpenAI API")
  }
  
  content <- fromJSON(rawToChar(response$content))
  return(content$choices$message$content)
}

# Helper function to extract SQL from text
extract_sql <- function(text) {
  # Pattern to match SQL between triple backticks, with multiline support
  sql_pattern <- "```(?:sql)?\\s*(.*?)\\s*```"
  matches <- regexpr(sql_pattern, text, perl = TRUE, useBytes = FALSE)
  
  if (matches != -1) {
    # Extract the captured group (the SQL code)
    sql <- regmatches(text, matches)[[1]]
    # Remove the backticks and any "sql" language identifier
    sql <- gsub("```(?:sql)?\\s*(.*?)\\s*```", "\\1", sql, perl = TRUE)
    return(trimws(sql))
  }
  
  # Fallback to look for SELECT statement
  sql_pattern <- "SELECT[\\s\\S]*?(?:;|$)"
  matches <- regexpr(sql_pattern, text, perl = TRUE)
  if (matches != -1) {
    sql <- regmatches(text, matches)[[1]]
    return(trimws(sql))
  }
  
  return(trimws(text))
}

# Helper function to extract values from OpenAI response
extract_value <- function(text, key) {
  str_extract(text, paste0(key, ": '(.*?)'")) %>%
    str_extract("'(.*?)'") %>%
    str_replace_all("'", "")
}

# Helper function to determine chart type
determine_chart_type <- function(question, data) {
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(
      "Authorization" = paste("Bearer", openai_api_key),
      "Content-Type" = "application/json"
    ),
    body = list(
      model = "gpt-4o-mini",
      messages = list(
        list(
          role = "system",
          content = paste0(
            "Given a question and data columns, suggest the most appropriate chart type and x and y axis. ",
            "Available columns: ", paste(colnames(data), collapse = ", "),
            "Return in this format:",
            "chart_type: 'bar'/'line'/'scatter'/'pie'",
            "x_axis: 'column_name'",
            "y_axis: 'column_name'",
            "title: 'Chart title'"
          )
        ),
        list(
          role = "user",
          content = question
        )
      )
    ),
    encode = "json"
  )
  
  content <- fromJSON(rawToChar(response$content))
  suggestion <- content$choices$message$content
  
  # Extract each value
  chart_type <- extract_value(suggestion, "chart_type")
  x_axis <- extract_value(suggestion, "x_axis")
  y_axis <- extract_value(suggestion, "y_axis")
  title <- extract_value(suggestion, "title")
  
  # Default to bar chart if no valid type is returned
  if (!chart_type %in% c("bar", "line", "scatter", "pie")) {
    chart_type <- "bar"
  }
  
  return(list(type = chart_type, x = x_axis, y = y_axis, title = title))
}

# UI definition
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .thinking-spinner {
        color: #666;
        font-style: italic;
        margin-top: 10px;
        margin-bottom: 10px;
      }
      .chat-message {
        margin-bottom: 15px;
        padding: 10px;
        border-radius: 5px;
      }
      .user-message {
        background-color: #f0f0f0;
      }
      .assistant-message {
        background-color: #f8f9fa;
      }
      .sql-query {
        background-color: #f8f9fa;
        padding: 10px;
        border-left: 3px solid #007bff;
        margin: 10px 0;
        font-family: monospace;
      }
      .dataTables_wrapper {
        margin: 20px 0;
        padding: 10px;
        border: 1px solid #ddd;
        border-radius: 5px;
      }
      .btn-primary {
        margin-left: 10px;
      }
      .export-buttons {
        margin-top: 10px;
      }
    "))
  ),
  titlePanel("Database Query Assistant"),
  
  sidebarLayout(
    sidebarPanel(
      # Database connection inputs
      textInput("host", "Host:", "178.62.193.245"),
      textInput("port", "Port:", "3306"),
      textInput("dbname", "Database:", "personale_ghi"),
      textInput("user", "Username:", "margino_dev"),
      passwordInput("password", "Password:", "h2tJuV3FnrnN8BsM"),
      actionButton("connect", "Connect to Database", class = "btn-primary"),
      hr(),
      # Connection status
      verbatimTextOutput("connection_status"),
      div(
        class = "export-buttons",
        downloadButton("export_chat_html", "Export Chat History as HTML")
        #downloadButton("export_chat_pdf", "Export Chat History as PDF")
      ),
      width = 3
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "output.is_connected == true",
        fluidRow(
          column(12,
            textInput("question", "", placeholder = "Ask a question about your data", width = '100%'),
            actionButton("send", "Send", class = "btn-primary"),
            div(
              style = "height: 680px; overflow-y: auto; border: 1px solid #ccc; padding: 1px; margin-bottom: 1px; width: 100%",
              id = "chat_history",
              uiOutput("chat_messages")
            ),
            uiOutput("thinking_spinner")
          )
        )
      )
    )
  )
)


# Server definition
server <- function(input, output, session) {
  # Reactive values
  rv <- reactiveValues(
    conn = NULL,
    messages = list(),
    is_connected = FALSE,
    db_schema = NULL
  )
  
  # Database connection
  observeEvent(input$connect, {
    tryCatch({
      rv$conn <- dbConnect(
        RMySQL::MySQL(),
        host = input$host,
        port = as.numeric(input$port),
        dbname = input$dbname,
        user = input$user,
        password = input$password
      )
      
      # Get database schema
      tables <- dbListTables(rv$conn)
      schema <- lapply(tables, function(table) {
        cols <- dbListFields(rv$conn, table)
        paste0(table, " (", paste(cols, collapse = ", "), ")")
      })
      rv$db_schema <- paste(schema, collapse = "\n")
      
      rv$is_connected <- TRUE
      showNotification("Connected to database successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Connection failed:", e$message), type = "error")
    })
  })
  
  # Connection status
  output$connection_status <- renderText({
    if (rv$is_connected) "Connected to database" else "Not connected"
  })
  
  output$is_connected <- reactive({
    rv$is_connected
  })
  outputOptions(output, "is_connected", suspendWhenHidden = FALSE)
  
  # Thinking spinner
  output$thinking_spinner <- renderUI({
    if (input$send > 0 && is.null(rv$messages[[length(rv$messages)]]$data)) {
      div(
        class = "thinking-spinner",
        "Thinking..."
      )
    }
  })
  
  # Handle chat messages
  observeEvent(input$send, {
    req(rv$conn, input$question)
    
    # Add user message
    rv$messages[[length(rv$messages) + 1]] <- list(
      role = "user",
      content = input$question,
      timestamp = Sys.time()
    )
    
    # Clear previous data for the current message
    rv$messages[[length(rv$messages)]]$data <- NULL
    
    tryCatch({
      # Generate SQL query
      sql_text <- generate_sql(input$question, rv$db_schema)
      sql_query <- extract_sql(sql_text)
      
      # Execute query
      data <- dbGetQuery(rv$conn, sql_query)
      
      # Generate visualization
      chart_info <- determine_chart_type(input$question, data)
      plot <- if (chart_info$type == "line") {
        plot_ly(data, x = as.formula(paste0("~", chart_info$x)), 
                y = as.formula(paste0("~", chart_info$y)), 
                type = 'scatter', mode = 'lines') %>%
          layout(title = chart_info$title)
      } else if (chart_info$type == "bar") {
        plot_ly(data, x = as.formula(paste0("~", chart_info$x)), 
                y = as.formula(paste0("~", chart_info$y)), 
                type = 'bar') %>%
          layout(title = chart_info$title)
      } else if (chart_info$type == "scatter") {
        plot_ly(data, x = as.formula(paste0("~", chart_info$x)), 
                y = as.formula(paste0("~", chart_info$y)), 
                type = 'scatter', mode = 'markers') %>%
          layout(title = chart_info$title)
      } else if (chart_info$type == "pie") {
        plot_ly(data, labels = as.formula(paste0("~", chart_info$x)), 
                values = as.formula(paste0("~", chart_info$y)), 
                type = 'pie') %>%
          layout(title = chart_info$title)
      }
      
      # Create DataTable object with improved formatting
      datatable_obj <- datatable(
        data,
        extensions = c('Buttons', 'Responsive'),
        options = list(
          dom = 'Bfrtip',
          buttons = list(
            list(extend = 'copy', className = 'btn-default'),
            list(extend = 'csv', className = 'btn-default'),
            list(extend = 'excel', className = 'btn-default'),
            list(extend = 'pdf', className = 'btn-default'),
            list(extend = 'print', className = 'btn-default')
          ),
          pageLength = 10,
          lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All'))
        ),
        class = 'cell-border stripe'
      )
      
      # Add assistant message
      rv$messages[[length(rv$messages) + 1]] <- list(
        role = "assistant",
        content = "Here's what I found:",
        sql = sql_query,
        data = data,
        plot = plot,
        datatable = datatable_obj,
        timestamp = Sys.time()
      )
      
    }, error = function(e) {
      rv$messages[[length(rv$messages) + 1]] <- list(
        role = "assistant",
        content = paste("Error:", e$message),
        timestamp = Sys.time()
      )
    })
    
    updateTextInput(session, "question", value = "")
  })
  
  # Render chat messages with improved formatting
  output$chat_messages <- renderUI({
    tagList(
      lapply(rv$messages, function(msg) {
        div(
          class = if(msg$role == "user") "chat-message user-message" else "chat-message assistant-message",
          div(
            style = "font-weight: bold;",
            if(msg$role == "user") "You:" else "Assistant:"
          ),
          div(msg$content),
          if (!is.null(msg$sql)) {
            div(
              class = "sql-query",
              pre(msg$sql)
            )
          },
          if (!is.null(msg$data)) {
            div(
              style = "margin: 10px 0;",
              DT::renderDataTable(msg$datatable)
            )
          },
          if (!is.null(msg$plot)) {
            div(
              style = "margin: 10px 0;",
              renderPlotly(msg$plot)
            )
          },
          div(
            style = "color: #666; font-size: 0.8em; margin-top: 5px;",
            format(msg$timestamp, "%Y-%m-%d %H:%M:%S")
          )
        )
      })
    )
  })
  
  # Export chat history as HTML with improved rendering
  output$export_chat_html <- downloadHandler(
    filename = function() {
      paste0("chat_history_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      # Save plots as static images
      plots <- lapply(rv$messages, function(msg) {
        if (!is.null(msg$plot)) {
          temp_file <- tempfile(fileext = ".png")
          export(msg$plot, file = temp_file)
          temp_file
        } else {
          NULL
        }
      })
      
      html_content <- tags$html(
        tags$head(
          tags$title("Chat History"),
          tags$link(rel = "stylesheet", href = "https://cdn.datatables.net/1.10.24/css/jquery.dataTables.css"),
          tags$script(src = "https://code.jquery.com/jquery-3.5.1.min.js"),
          tags$script(src = "https://cdn.datatables.net/1.10.24/js/jquery.dataTables.min.js")
        ),
        tags$body(
          lapply(seq_along(rv$messages), function(i) {
            msg <- rv$messages[[i]]
            div(
              class = if(msg$role == "user") "chat-message user-message" else "chat-message assistant-message",
              h4(if(msg$role == "user") "You:" else "Assistant:"),
              p(msg$content),
              if (!is.null(msg$sql)) pre(class = "sql-query", msg$sql),
              if (!is.null(msg$data)) {
                div(
                  tags$table(
                    class = "table table-striped",
                    tags$thead(
                      tags$tr(lapply(names(msg$data), tags$th))
                    ),
                    tags$tbody(
                      apply(msg$data, 1, function(row) {
                        tags$tr(lapply(row, tags$td))
                      })
                    )
                  )
                )
              },
              if (!is.null(msg$plot) && !is.null(plots[[i]])) {
                img(src = plots[[i]], width = "100%")
              },
              p(class = "timestamp", format(msg$timestamp, "%Y-%m-%d %H:%M:%S"))
            )
          }),
          tags$script(HTML("
  $(document).ready(function() {
    $('table').DataTable();
  });
"))
        )
      )
      
      save_html(html_content, file)
    }
  )
  
    # Export chat history as PDF
  # output$export_chat_pdf <- downloadHandler(
  #   filename = function() {
  #     paste("chat_history", Sys.Date(), ".pdf", sep = "")
  #   },
  #   content = function(file) {
  #     temp_report <- file.path(tempdir(), "report.Rmd")
  #     file.copy("report_template.Rmd", temp_report, overwrite = TRUE)
  #     
  #     params <- list(messages = rv$messages)
  #     
  #     out <- rmarkdown::render(temp_report, params = params, envir = new.env(parent = globalenv()))
  #     file.rename(out, file)
  #   }
  # )
  
  # Cleanup on session end
  observe({
    session$onSessionEnded(function() {
      conn <- isolate(rv$conn)
      if (!is.null(conn)) {
        tryCatch({
          dbDisconnect(conn)
        }, error = function(e) {
          warning("Error disconnecting from database: ", e$message)
        })
      }
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)