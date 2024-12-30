
import streamlit as st
import pandas as pd
from sqlalchemy import create_engine, inspect, text
import json

class DatabaseManager:
    def connect(self, connection_string):
        """Connect to database and get schema of all tables"""
        try:
            engine = create_engine(connection_string)
            
            # Test connection
            with engine.connect() as conn:
                conn.execute(text("SELECT 1"))
            
            st.session_state.db_connection = engine
            
            # Get and store schema of all tables
            schema = self.get_database_schema(engine)
            if schema:
                st.session_state.schema = schema
                # Debug: Print the schema
                st.write("Debug - Connected successfully. Schema:", schema)
                return True
            return False
        except Exception as e:
            st.error(f"Error connecting to database: {str(e)}")
            return False

    def get_database_schema(self, engine):
        """Get the schema of all tables in the database"""
        try:
            inspector = inspect(engine)
            schema_dict = {}
            
            # Get all tables
            for table_name in inspector.get_table_names():
                # Get table details
                columns = []
                for column in inspector.get_columns(table_name):
                    columns.append({
                        "name": column['name'],
                        "type": str(column['type'])
                    })
                
                # Get primary key info
                pk_constraint = inspector.get_pk_constraint(table_name)
                if pk_constraint and 'constrained_columns' in pk_constraint:
                    for col in columns:
                        if col['name'] in pk_constraint['constrained_columns']:
                            col['is_primary_key'] = True
                
                # Get foreign key info
                fk_list = inspector.get_foreign_keys(table_name)
                for fk in fk_list:
                    for col in columns:
                        if col['name'] in fk['constrained_columns']:
                            col['foreign_key'] = {
                                'referred_table': fk['referred_table'],
                                'referred_columns': fk['referred_columns']
                            }
                
                schema_dict[table_name] = {
                    'columns': columns,
                    'sample_data': self.get_sample_data(engine, table_name)
                }
            
            return schema_dict
        except Exception as e:
            st.error(f"Error getting schema: {str(e)}")
            return None

    def get_sample_data(self, engine, table_name, limit=2):
        """Get sample data from the table"""
        try:
            query = f"SELECT * FROM {table_name} LIMIT {limit}"
            df = pd.read_sql_query(text(query), engine)
            return df.to_dict(orient='records')
        except Exception as e:
            st.error(f"Error getting sample data for {table_name}: {str(e)}")
            return []

    def execute_query(self, query):
        """Execute SQL query and return results as DataFrame"""
        try:
            # Debug: Print the query being executed
            st.write("Debug - Executing query:", query)
            
            return pd.read_sql_query(text(query), st.session_state.db_connection)
        except Exception as e:
            st.error(f"Error executing query: {str(e)}")
            return None

    def disconnect(self):
        """Disconnect from database and clear session state"""
        st.session_state.db_connection = None
        st.session_state.messages = []
        st.session_state.schema = None
        st.experimental_rerun()