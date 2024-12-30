import openai
import json
import streamlit as st

class LLMManager:
    def generate_sql_query(self, question, schema):
        """Generate SQL query using OpenAI's GPT model"""
        try:
            # Format schema for better prompt
            formatted_schema = "Available tables and their columns:\n"
            for table_name, table_info in schema.items():
                formatted_schema += f"\nTable: {table_name}\n"
                formatted_schema += "Columns:\n"
                for col in table_info['columns']:
                    formatted_schema += f"- {col['name']} ({col['type']})"
                    if col.get('is_primary_key'):
                        formatted_schema += " (Primary Key)"
                    if col.get('foreign_key'):
                        formatted_schema += f" (Foreign Key -> {col['foreign_key']['referred_table']})"
                    formatted_schema += "\n"

            system_prompt = f"""You are a SQL expert. Given a database schema, generate a valid SQL query to answer the user's question.

Schema Information:
{formatted_schema}

Rules for query generation:
1. Always start with SELECT
2. Only use tables and columns that exist in the schema
3. Use table name prefixes for columns when joining tables
4. Add appropriate JOIN conditions when using multiple tables
5. Include a semicolon at the end of the query
6. Keep the query focused and specific to the question
7. Use appropriate WHERE clauses to filter data
8. Use appropriate aggregations (COUNT, SUM, AVG, etc.) when needed
"""

            user_prompt = f"""Question: {question}

Generate a SQL query to answer this question using the schema provided.
Remember to use only the tables and columns that exist in the schema.
Return only the SQL query, nothing else."""

            # Debug outputs
            st.write("Debug - Schema:", formatted_schema)
            st.write("Debug - Question:", question)

            # Use the ChatCompletion API correctly
            response = openai.ChatCompletion.create(
                model="gpt-4",
                messages=[
                    {"role": "system", "content": system_prompt},
                    {"role": "user", "content": user_prompt}
                ],
                temperature=0.1,
                max_tokens=500
            )

            query = response['choices'][0]['message']['content'].strip()  # Correct access method
            
            # Debug output
            st.write("Debug - Generated Query:", query)

            # Validate the query format
            if not query.lower().startswith('select'):
                st.write("Debug - Query validation failed: Does not start with SELECT")
                return None

            if not query.rstrip().endswith(';'):
                query += ';'

            # Basic SQL injection prevention
            dangerous_keywords = ['drop', 'delete', 'truncate', 'update', 'insert', 'alter', 'create']
            if any(keyword in query.lower() for keyword in dangerous_keywords):
                st.write("Debug - Query validation failed: Contains dangerous keywords")
                return None

            return query

        except Exception as e:
            st.error(f"Error in generate_sql_query: {str(e)}")
            if hasattr(e, 'response'):
                st.error(f"API Response: {e.response}")
            return None

    def suggest_visualization(self, df, question):
        """Use GPT to suggest appropriate visualization"""
        try:
            data_description = f"""
            DataFrame info:
            - Shape: {df.shape}
            - Columns: {', '.join(df.columns)}
            - Data types: {df.dtypes.to_dict()}
            - Sample data: {df.head(2).to_dict()}
            """

            system_prompt = """You are a data visualization expert. Suggest the most appropriate visualization for the given data and question.
            Return a JSON object with the following structure:
            {
                "chart_type": "one of [line, bar, scatter, pie, histogram, box, heatmap]",
                "x": "column name for x-axis",
                "y": "column name for y-axis",
                "title": "suggested chart title",
                "color": "column name for color differentiation (optional)"
            }
            """

            response = openai.ChatCompletion.create(
                model="gpt-4",
                messages=[
                    {"role": "system", "content": system_prompt},
                    {"role": "user", "content": f"Question: {question}\n\nData: {data_description}"}
                ],
                temperature=0.1
            )
            
            return json.loads(response.choices[0].message.content)
        except Exception as e:
            st.error(f"Error suggesting visualization: {str(e)}")
            return None