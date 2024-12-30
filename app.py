import streamlit as st
from langchain_openai import ChatOpenAI
from langchain_community.utilities import SQLDatabase
from langchain.chains import create_sql_query_chain
import pandas as pd
import sqlalchemy as sql
import yaml
import re
import plotly.express as px
import os
from dotenv import load_dotenv

# Page config
st.set_page_config(page_title="Database Query Assistant", layout="wide")

# Initialize session state variables
if 'connected' not in st.session_state:
    st.session_state.connected = False
if 'db_connection' not in st.session_state:
    st.session_state.db_connection = None
if 'messages' not in st.session_state:
    st.session_state.messages = []

def load_credentials():
    """Load OpenAI API key from .env file"""
    load_dotenv()
    os.environ["OPENAI_API_KEY"] = os.getenv('OPENAI_API_KEY')
    return ChatOpenAI(model="gpt-4o-mini")

def extract_sql_code(response):
    """
    Extracts SQL code from the response of the language model.

    Args:
        response: The response object from the language model.

    Returns:
        str: The extracted SQL code in a single line.
    """
    # Check if response is a string or an object with a content attribute
    if isinstance(response, str):
        sql_code = response.strip()
    elif hasattr(response, 'content'):
        sql_code = response.content.strip()
    else:
        raise ValueError("Response does not contain valid content.")

    # Remove unwanted prefixes like "SQLQuery:" and "```sql"
    sql_code = sql_code.replace("SQLQuery:", "").replace("```sql", "").strip()

    # Remove the trailing ";\n" if it exists
    if sql_code.endswith("```"):
        sql_code = sql_code[:-3].strip()  # Remove trailing ";"

    # Replace escaped newlines with actual newlines
    sql_code = sql_code.replace("\\n", "\n").strip()

    # Remove actual newlines and extra spaces to make it a single line
    sql_code = ' '.join(sql_code.split())  # Split by whitespace and join with a single space

    # Replace single '%' with '%%'
    sql_code = sql_code.replace('%', '%%')

    # Remove any trailing backticks or other unwanted characters
    sql_code = sql_code.rstrip('` ')  # Remove trailing backticks and spaces

    return sql_code

def connect_to_database(connection_string):
    """Attempt to connect to the database"""
    try:
        if not connection_string.startswith('mysql+pymysql://'):
            return False, "Invalid connection string format. Must start with mysql+pymysql://"
        
        engine = sql.create_engine(connection_string)
        with engine.connect() as conn:
            conn.execute(sql.text('SELECT 1'))
        return True, engine
    except Exception as e:
        return False, f"Database connection failed: {str(e)}"

def analyze_chart_question(question, df, llm):
    """Analyze question to determine appropriate chart type"""
    prompt = f"""Given the question: '{question}', suggest the most appropriate chart type and axes based on the data structure {df.columns.tolist()}.
    Return in this format:
    chart_type: 'bar'/'line'/'scatter'/'pie'/'donut'
    x_axis: 'column_name'
    y_axis: 'column_name'
    title: 'Chart title'
    """
    response = llm.invoke(prompt)
    chart_type = re.search(r"chart_type:\s*'(.*?)'", response.content).group(1)
    x_axis = re.search(r"x_axis:\s*'(.*?)'", response.content).group(1)
    y_axis = re.search(r"y_axis:\s*'(.*?)'", response.content).group(1)
    title = re.search(r"title:\s*'(.*?)'", response.content).group(1)
    
    return {
        "chart_type": chart_type,
        "x_axis": x_axis,
        "y_axis": y_axis,
        "title": title
    }

def generate_chart(df, chart_analysis):
    """Generate an appropriate chart based on the analysis"""
    try:
        chart_type = chart_analysis.get('chart_type', 'bar')
        x_column = chart_analysis.get('x_axis')
        y_column = chart_analysis.get('y_axis')
        chart_title = chart_analysis.get('title', 'Data Visualization')
        
        if chart_type == 'bar':
            fig = px.bar(df, x=x_column, y=y_column, title=chart_title)
        elif chart_type == 'line':
            fig = px.line(df, x=x_column, y=y_column, title=chart_title)
        elif chart_type == 'scatter':
            fig = px.scatter(df, x=x_column, y=y_column, title=chart_title)
        elif chart_type in ['pie', 'donut']:
            fig = px.pie(df, values=y_column, names=x_column, title=chart_title)
            if chart_type == 'donut':
                fig.update_traces(hole=0.4)
        
        return fig
    except Exception as e:
        st.error(f"Failed to generate chart: {str(e)}")
        return None

def main():
    st.title("Database Query Assistant")
    
    # Load OpenAI LLM
    llm = load_credentials()
    
    # Database Connection Section
    if not st.session_state.connected:
        st.subheader("Database Connection")
        connection_string = st.text_input(
            "Enter your MySQL connection string:",
            placeholder="mysql+pymysql://username:password@host:port/database"
        )
        
        if st.button("Connect"):
            success, result = connect_to_database(connection_string)
            if success:
                st.session_state.db_connection = result
                st.session_state.connected = True
                st.success("Successfully connected to database!")
                st.rerun()
            else:
                st.error(result)
    
    # Chat Interface
    else:
        # Sidebar with connection status and disconnect button
        with st.sidebar:
            st.success("Connected to database")
            if st.button("Disconnect"):
                st.session_state.connected = False
                st.session_state.db_connection = None
                st.session_state.messages = []
                st.rerun()
        
        # Display chat messages
        for message in st.session_state.messages:
            with st.chat_message(message["role"]):
                st.write(message["content"])
                if "sql" in message:
                    st.code(message["sql"], language="sql")
                if "data" in message:
                    st.dataframe(message["data"])
                if "chart" in message:
                    st.plotly_chart(message["chart"], use_container_width=True)
        
        # Chat input
        if prompt := st.chat_input("Ask a question about your data"):
            # Add user message to chat
            st.session_state.messages.append({"role": "user", "content": prompt})
            
            # Process the question
            try:
                # Create database utility
                db = SQLDatabase.from_uri(st.session_state.db_connection.url)
                sql_generator = create_sql_query_chain(llm=llm, db=db, k=int(1e7))
                
                # Generate SQL query
                sql_response = sql_generator.invoke({"question": prompt})
                if sql_response is None:
                    st.error("Failed to generate SQL query. The model returned no response.")
                    return
                
                sql_query = extract_sql_code(sql_response)
                if not sql_query:
                    st.error("Failed to extract valid SQL query from the response.")
                    return
                
                # Log the query for debugging
                #st.debug(f"Generated SQL query: {sql_query}")
                
                try:
                    # Execute query and get results
                    df = pd.read_sql(sql_query, st.session_state.db_connection)
                except Exception as e:
                    st.error(f"Error executing SQL query: {str(e)}")
                    st.code(sql_query, language="sql")  # Show the problematic query
                    return
                
                # Generate chart
                chart_analysis = analyze_chart_question(prompt, df, llm)
                chart = generate_chart(df, chart_analysis)
                
                # Add assistant response to chat
                st.session_state.messages.append({
                    "role": "assistant",
                    "content": "Here's is the result to your query:",
                    "sql": sql_query.strip(),
                    "data": df,
                    "chart": chart
                })
                
                st.rerun()
                
            except Exception as e:
                st.error(f"Error processing question: {str(e)}")

if __name__ == "__main__":
    main()