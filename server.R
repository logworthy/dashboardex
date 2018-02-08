# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)

shinyServer(function(input, output) {
    
  # identify the clicked-on point
  getSelection <- function() {
    # read the current values of the input variables
    # but dont watch them; only want to trigger on graph clicks
    # for testing:
    # Species <- 'setosa'; Metric <- 'Sepal.Width'
    isolate({
    i.Species <- input$Species
    i.Metric <- input$Metric
    i.Period <- input$Period
    click <- input$plot_click
    })  
    
    in_scope_ids <- id_mapping[id_mapping[[i.Period]], id]
    
    id_vals <- metrics_long[
      id %in% in_scope_ids &
      Species==i.Species & 
        Metric==i.Metric, list(id, Value)
      ]
    
    # for testing:
    # click <- list(x=13.49, y=4)
    
    # calculate distance from each point
    id_vals[, dist := (id - click$x)**2 + (Value-click$y)**2]
    
    # take the nearest distance.  break ties if needed
    best_fit <- id_vals[dist==min(dist)][1,]
    
    # clear any old flags, flag selected point as clicked
    id_mapping[, clicked := F]
    id_mapping[id==best_fit$id, clicked := T]
    
    invisible(NULL)
    
  }
  
  # update the metric plot (e.g. to highlight points, or change species etc. that has been displayed)
  updateMetricPlot <- function() {
    
    isolate({
    i.Species <- input$Species
    i.Metric <- input$Metric
    i.Period <- input$Period
    })
    
    in_scope_ids <- id_mapping[id_mapping[[i.Period]], id]
    
    output$MetricPlot <- renderPlotly({
      
      p <- ggplot(
        metrics_long[
          id %in% in_scope_ids &
          Species==i.Species & 
          Metric==i.Metric
        ], 
        aes(x=id, y=Value)
      )+
      geom_line()+
      geom_point(data=metrics_long[
        id %in% in_scope_ids &
        Species==i.Species & 
          Metric==i.Metric & 
          id %in% id_mapping[clicked==T, id]
      ], color='red')
      
      ggplotly(p)
  
    })
  }
  
  # update the indicator table
  updateIndicatorTable <- function() {
    
    isolate({
      i.Species <- input$Species
      i.Metric <- input$Metric
      i.Period <- input$Period
    })
    
    in_scope_ids <- id_mapping[id_mapping[[i.Period]], id]
    
    output$IndicatorTable <- renderTable({
      
      related_metrics <- driver_reln_all[Parent_Metric == i.Metric, Metric]
      
      metrics_long[
          id %in% in_scope_ids &
            Species==i.Species & 
            Metric %in% related_metrics & 
            id %in% id_mapping[clicked==T, id],
          list(Metric, Value)
      ]
      
    })
  }
  
  # update the metric plot (e.g. to highlight points, or change species etc. that has been displayed)
  updateBreakdownPlot <- function() {
    
    isolate({
      i.Species <- input$Species
      i.Metric <- input$Metric
      i.Period <- input$Period
    })
    
    in_scope_ids <- id_mapping[id_mapping[[i.Period]], id]
    
    output$BreakdownPlot <- renderPlot({
      
      related_metrics <- graph_reln_all[Parent_Metric == i.Metric, list(Metric, display_name)]
      
      graph_data <- merge(
        related_metrics,
        metrics_long[
          id %in% in_scope_ids &
            Species==i.Species &
            id %in% id_mapping[clicked==T, id]
        ],
        by.x='Metric',
        by.y='Metric'
      )[, list(Category=display_name, Value)]
      
      print(graph_data)
      
      ggplot(
        graph_data, 
        aes(x=Category, y=Value)
      )+geom_bar(stat='identity')
      
    })
  }
  
  # if someone clicks, find out what they clicked on and update the plot
  observeEvent(input$plot_click, {
    getSelection()
    updateMetricPlot()
    updateIndicatorTable()
    updateBreakdownPlot()
  })
  
  # if a slider changes, update the plot
  observeEvent({input$Species; input$Metric; input$Period}, {
    updateMetricPlot()
    updateIndicatorTable()
    updateBreakdownPlot()
  })
  
})
