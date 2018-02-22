
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("ROC Example"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("auc",
                  "Area Under the Curve:",
                  min = .5,
                  max = 1,
                  value = .8,
                  step = .05),
#       numericInput("n",
#                    "Number of Observations:",
#                    value = 1000, min = 10, max = 10000),
      radioButtons("n","Number of Observations:",
                   choices=c("10"=10,"100"=100,"1,000"=1000,"10,000"=10000), selected = 1000),
      sliderInput("pos_rate",
                   "Rate of Outcome:",
                   .3, min =.05, max=.95,step=.05),
      sliderInput("threshold",
                   "Prediction Threshold",
                   .3, min=0, max=1, step = .05)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("wafflePOS"),
      plotOutput("waffleNEG"),
      plotOutput("histplot"),
      splitLayout(cellWidths = c("40%", "60%"), div(tableOutput("conf_table"),style="font-size:130%"),
      plotOutput("ROCPlot"))
    )
  )
))
