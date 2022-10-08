##### server.R 
library(shiny)
library(tidyverse)
library(zoo)

covid <- read_csv("COVID-19_Daily_Counts_of_Cases__Hospitalizations__and_Deaths.csv")


shinyServer(function(input, output) {
  
  
  output$plot=renderPlot({
    covid_date <- covid %>% mutate(date_of_interest = as.Date(date_of_interest, "%m/%d/%Y")) %>%
      select(date_of_interest, CASE_COUNT, HOSPITALIZED_COUNT, DEATH_COUNT) %>%
      filter(date_of_interest >= as.Date(input$dates[1]) & date_of_interest <= as.Date(input$dates[2])) %>%
      pivot_longer(cols=c(`CASE_COUNT`, `HOSPITALIZED_COUNT`, `DEATH_COUNT`), names_to = "variable", values_to = "count")
    
    
    ggplot(data=covid_date, aes(x=date_of_interest, y=count)) + 
      geom_line(aes(color=variable)) + 
      scale_x_date(date_breaks  ="2 month") +
      labs(x="Date of Interest", 
           y="Count") +
      scale_colour_manual(values=c("darkcyan", "black", "coral")) +
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size=5), 
            legend.text = element_text(size=6), legend.title = element_text(size=8))
    
    
    
  })
})

#### ui.R 
library(shiny)

shinyUI(fluidPage(
  
  dateRangeInput("dates", label = h3("Date range")),
  
  hr(),
  fluidRow(column(4, verbatimTextOutput("value"))), 
  
  mainPanel(
    plotOutput("plot")))
  
)