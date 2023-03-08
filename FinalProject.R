library(shiny)
library(tidyverse)
library(ggplot2)

data <- read_delim("Rail_Equipment_Accident_Incident_Data.csv")

ui <- fluidPage(
  titlePanel("Info 201 Final project"),
  tabsetPanel(
    tabPanel(
      "Home",
      img(src="https://media.npr.org/assets/img/2023/02/05/ap23035653276274-f6d38396bc1cd8b060e6801e2506f98b047f62bf-s800-c85.webp",height=400,width=400),
      h1("Project Overview"),
      p("The railroad industry is a crucial part of transportation infrastructure, but it is not without risk. Railroad accidents and incidents can result in significant harm to human life, the environment, and the economy. Understanding the patterns and causes of these incidents is critical for improving safety and preventing future accidents.
        This project aims to explore the available railroad accident and incident data and use statistical analysis techniques to gain insights into the factors that influence these events."),
      h2("Audience"),
      p("The audience for this project may include individuals and organizations involved in the railroad industry, such as railroad companies, regulators, and safety organizations. It may also include researchers and analysts interested in transportation safety or statistical analysis of complex datasets. The insights gained from this project 
        may be relevant to policymakers and decision-makers involved in transportation safety at the local, national, or international level."),
      h2("Question"),
      p("Some questions we are focues on are:"),
      tags$ul(
        tags$li("How does weather affects accidents?"),
        tags$li("What is the cost impact of accidents?"),
        tags$li("question 3 ")
      ),
      h2("Railroad Accident & Incident Data"),
      p("Safety data related to Railway Equipment Failures and Accidents"),
      p("We will analyze railroad accident data to determine causes and impacts of railroad accidents."),
      a("https://www.kaggle.com/datasets/chrico03/railroad-accident-and-incident-data"),
      p("Dataset published by the Federal Railroad Administration, Office of Railroad Safety; contains data on railway incidents from 1975 to 2022."),
      p("Includes data on:"),
      tags$ul(
        tags$li("Railway company involved"),
        tags$li("Hazmat cars involved"),
        tags$li("Number of people evacuated"),
        tags$li("Number of employees on-duty")
      ),
      textOutput("nrow"),
      textOutput("ncol"),
      p(""),
      strong("Sample data"),
      tags$div(style="height: 600px; overflow-x: scroll; width: 1600px; overflow-y: scroll;", tableOutput("sample_table")),
      h2("Creators"),
      tags$ul(
        tags$li("Karina Yang"),
        tags$li("Caleb Lee"),
        tags$li("Lily Bates"),
        tags$li("Joe Wicorek")
      ),
    ),
    
    tabPanel(
      "Joe's Panel",
      mainPanel(
        "Joe's project here"
      )
    ),
    
    tabPanel(
      "Weather and Visibility Trends",
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
                       selected = "Yes") ## button for selecting time of day
        ),
        mainPanel(
          plotOutput("weather_graph"), ## graph output from user input
          textOutput("weatherobs"), ## note about how many observations user is seeing
          p(" "),
          p("This graph shows that the majority of crashes happen in clear weather
            condtions and when it is dark and visibility is low. Overall,
            the most common type of crash is a derailment followed by 
            highway/railroad crossings. During clear weather, more accidents 
            happen in the daytime compared to other weather types. Though this 
            coorelation may be due to the high prevelance of clear weather, it may 
            also suggest that less caution is taken in clear, high visibility
            conditions, leading to more accidents.")
        ))
    ), 
    
    
    tabPanel(
      "Track Type and State Analysis",
      sidebarLayout(
        sidebarPanel(
          p("Observe the cost of damages on incidents."),
          radioButtons("cost", 
                       "Choose which State to view",
                       choices = unique(data$`State Name`),
                       selected = "Clear"), ## button for selecting weather
        ),
        mainPanel(
          plotOutput("cost_graph"),
          textOutput("totalCount")
        )
      )),
    tabPanel(
      "Summary",
      mainPanel(
        p("summary info here")
      )
    ), 
  ))


server <- function(input, output) {
  
  #calc the number of rows and columns and their output functions
  nrow <- nrow(data)
  output$nrow <- renderText(paste("The number of rows is:", nrow))
  ncol <- ncol(data)
  output$ncol <- renderText(paste("The number of columns is:", ncol))
  
  #get some data to display with output function
  sample_data <- data[sample(nrow(data), 10), ]
  output$sample_table <- renderTable(sample_data, options = list(scrollX = TRUE, scrollY = "300px"))  
  
  
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
      cat("There were", . , "total collisions in the selected weather type.")
  }) ## code for text output of how many observations user is seeing in weather crash graph
  
  costgraph <- reactive({
    data %>% 
      filter(`State Name` %in% input$cost) %>% 
      filter(`Report Year` >= 1990)
  }) 
  
  output$cost_graph <- renderPlot ({
    ggplot(costgraph(), (aes (x = `Track Type`)))+
      geom_histogram(stat = "count")
  })
  
  output$totalCount <- renderPrint({
    data %>% 
      filter(`State Name` %in% input$cost) %>% 
      filter(`Report Year` >= 1990) %>%  
      nrow() %>% 
      cat("The cost was", . , "in this state.")
  })
}


shinyApp(ui = ui, server = server)