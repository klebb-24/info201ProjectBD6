
library(shiny)
library(tidyverse)
library(ggplot2)

ui <- fluidPage(
    titlePanel("Info 201 Final project"),
    tabsetPanel(
      tabPanel(
        "Introduction Page",
        mainPanel(
          p("this will introduce problem and dataset")
        )
      ),
    
  tabPanel(
    "Joe's Panel",
    mainPanel(
      "Joe's project here"
    )
  ),
  
  tabPanel(
    "Lily's Panel",
    mainPanel(
      "Lily's project here"
    )
  ), 
  
  tabPanel(
    "Karina's Panel",
    mainPanel(
      "Karina's project here"
    )
  ), 
  
  tabPanel(
    "Kaleb's Panel",
    mainPanel(
      "Kalebs's project here"
    )
  )
))


server <- function(input, output) {

}


shinyApp(ui = ui, server = server)
