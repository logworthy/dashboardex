

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)

shinyUI(fluidPage(

  # header & product selectors
  fluidRow(
    column(8, h1("The Dashboard Title")),
    column(4,
        selectInput("Species",
                   "Species to show:",
                   choices=unique(as.character(metrics_long$Species))
        ),
        selectInput("Period",
                    "Period",
                    choices=setdiff(colnames(id_mapping), c('clicked', 'id')))
      )
  ),

  # metric choice & plot
  fluidRow(
    column(4,
      
      selectInput("Metric",
                  "Metric to show:",
                  choices=unique(as.character(metrics_long$Metric))
                  )

    ),

    # Show a plot of the generated distribution
    column(8,
      plotlyOutput("MetricPlot", click="plot_click")
    )
  ),
  
  # table and metric graphs
  fluidRow(
    column(7,
      tableOutput('IndicatorTable')
    ),
    column(5,
      plotOutput('BreakdownPlot')
    )
  )
  
))
