
library(shiny)
library(tidyverse)
library(ggplot2)

data <- read_delim("Rail_Equipment_Accident_Incident_Data.csv")

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
    "Lily's Panel - Weather and Visibility Trends",
    sidebarLayout(
      sidebarPanel(
        p("Observe the effect on weather on incidents."),
        radioButtons("weather", 
                     "Choose weather type to veiw",
                           choices = unique(data$`Weather Condition`),
                           selected = "Clear"), ## button for selecting weather
      radioButtons("vis_button",
       "Display time of day accident occured?",
        choices = c("Yes", "No"),
        selected = "No") ## button for selecting time of day
      ),
    mainPanel(
      plotOutput("weather_graph"), ## graph output from user input
      textOutput("weatherobs") ## note about how many observations user is seeing
    ))
  ), 
  
  tabPanel(
    "Karina's Panel",
    mainPanel(
      "Karina's project here"
    )
  ), 
  
  tabPanel(
    "Caleb's Panel - Damage Cost Analysis",
    sidebarLayout(
      sidebarPanel(
        p("Observe the cost of damages on incidents."),
        selectInput("cost_type", "Select cost type to view",
                    choices = c("Equipment Damage Cost", "Track Damage Cost", "Total Damage Cost"),
                    selected = "Equipment Damage Cost")
      ),
      mainPanel(
        plotOutput("cost_graph")
      )
    )),
))


server <- function(input, output) {
  
weathergraph <- reactive({
  data %>% 
    filter(`Weather Condition` %in% input$weather) %>% 
    filter(`Report Year` == 2022)
})  ## weather crash graph 

output$weather_graph <- renderPlot ({
  if(input$vis_button == "Yes")({
    ggplot(weathergraph(), (aes (y= `Accident Type`, fill = factor(Visibility))))+
      geom_histogram(stat = "count")
  })else ({
    ggplot(weathergraph(), (aes (y= `Accident Type`)))+
    geom_histogram(stat = "count")
    }) 
}) ## time of day button for weather crash graph 

output$weatherobs <- renderPrint({
  data %>% 
    filter(`Weather Condition` %in% input$weather) %>% 
    filter(`Report Year` == 2022) %>%  
    nrow() %>% 
    p("There were", . , "total collisions in the selected weather type.")
}) ## code for text output of how many observations user is seeing in weather crash graph
}


shinyApp(ui = ui, server = server)


