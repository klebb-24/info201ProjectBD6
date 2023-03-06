
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
                           selected = "Clear"),
      radioButtons("vis_button",
       "Display time of day accident occured?",
        choices = c("Yes", "No"),
        selected = "No")
      ),
    mainPanel(
      plotOutput("weather_graph"),
      textOutput("weatherobs")
    ))
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
  
weathergraph <- reactive({
  data %>% 
    filter(`Weather Condition` %in% input$weather) %>% 
    filter(`Report Year` == 2022)
})

output$weather_graph <- renderPlot ({
  if(input$vis_button == "Yes")({
    ggplot(weathergraph(), (aes (y= `Accident Type`, fill = factor(Visibility))))+
      geom_histogram(stat = "count")
  })else ({
    ggplot(weathergraph(), (aes (y= `Accident Type`)))+
    geom_histogram(stat = "count")
    })
})

output$weatherobs <- renderPrint({
  data %>% 
    filter(`Weather Condition` %in% input$weather) %>% 
    filter(`Report Year` == 2022) %>%  
    nrow() %>% 
    p("There were", . , "total collisions in the selected weather type.")
})
}


shinyApp(ui = ui, server = server)


