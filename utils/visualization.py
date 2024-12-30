import streamlit as st
import plotly.express as px

class VisualizationManager:
    def create_visualization(self, df, viz_config):
        """Create visualization based on GPT suggestion"""
        try:
            chart_type = viz_config['chart_type'].lower()
            
            if chart_type == 'line':
                fig = px.line(df, x=viz_config['x'], y=viz_config['y'], 
                             title=viz_config['title'],
                             color=viz_config.get('color'))
            
            elif chart_type == 'bar':
                fig = px.bar(df, x=viz_config['x'], y=viz_config['y'], 
                            title=viz_config['title'],
                            color=viz_config.get('color'))
            
            elif chart_type == 'scatter':
                fig = px.scatter(df, x=viz_config['x'], y=viz_config['y'], 
                               title=viz_config['title'],
                               color=viz_config.get('color'))
            
            elif chart_type == 'pie':
                fig = px.pie(df, values=viz_config['y'], names=viz_config['x'], 
                            title=viz_config['title'])
            
            elif chart_type == 'histogram':
                fig = px.histogram(df, x=viz_config['x'], 
                                 title=viz_config['title'])
            
            elif chart_type == 'box':
                fig = px.box(df, x=viz_config['x'], y=viz_config['y'], 
                            title=viz_config['title'],
                            color=viz_config.get('color'))
            
            elif chart_type == 'heatmap':
                fig = px.imshow(df.pivot(index=viz_config['y'], 
                                       columns=viz_config['x'], 
                                       values=viz_config.get('color')),
                               title=viz_config['title'])
            
            else:
                st.warning(f"Unsupported chart type: {chart_type}")
                return
            
            st.plotly_chart(fig)
        
        except Exception as e:
            st.error(f"Error creating visualization: {str(e)}")
            st.dataframe(df)