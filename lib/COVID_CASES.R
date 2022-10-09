##### server.R 
library(shiny)
library(tidyverse)
library(zoo)

covid <- read_csv("COVID-19_Daily_Counts_of_Cases__Hospitalizations__and_Deaths.csv")
head(covid)

server <- function(input, output) {
  
  output$plot1=renderPlot({
    covid_ordered <- covid[order(as.Date(covid$date_of_interest, format = "%m/%d/%Y")),]
    covid_order_selectcols<- select(covid_ordered, CASE_COUNT, HOSPITALIZED_COUNT, DEATH_COUNT, date_of_interest)
    covid_order_selectcols$dateandtime <- as.Date(as.POSIXct(covid_order_selectcols$date_of_interest, format = "%m/%d/%Y"))
    covid_ordered_filtered <- subset(covid_order_selectcols, dateandtime >= input$dates[1] & dateandtime <= input$dates[2])
    diff_dates = difftime(input$dates[2],input$dates[1], units = "days")
    diff_dates = as.numeric(diff_dates)
    covid_ordered_filtered$dateandtime <- as.POSIXct(covid_ordered_filtered$date_of_interest,format = "%m/%d/%Y")
    
    daterange=c(as.POSIXlt(min(covid_ordered_filtered$dateandtime)), as.POSIXlt(max(covid_ordered_filtered$dateandtime)))
    
    if (diff_dates > 31) {
      plot(covid_ordered_filtered$dateandtime, covid_ordered_filtered$CASE_COUNT, type = "l", col = "blue" , xaxt= "n", xlab = "",
           , ylab = "Cases", main = "Cases")
      points(covid_ordered_filtered$dateandtime, covid_ordered_filtered$CASE_COUNT, 
             type = "p", xaxt= "n", xlab = "", color = "blue")
      
      axis.POSIXct(1, at=seq(daterange[1], daterange[2], by = "1 month"), format="%m-%d-%Y",las = 2)
    }
    
    if (diff_dates <= 31) {
      plot(covid_ordered_filtered$dateandtime, covid_ordered_filtered$CASE_COUNT, type = "l", col = "blue" , xaxt= "n", xlab = ""
           , ylab = "Cases", main = "Cases")
      points(covid_ordered_filtered$dateandtime, covid_ordered_filtered$CASE_COUNT, 
             type = "p", xaxt= "n", xlab = "", col = "blue")
  
      axis.POSIXct(1, at=seq(daterange[1], daterange[2], by = "1 day"), format="%m-%d-%Y",las = 2)
    }
    })
    
    
    output$plot2=renderPlot({
      covid_ordered <- covid[order(as.Date(covid$date_of_interest, format = "%m/%d/%Y")),]
      covid_order_selectcols<- select(covid_ordered, CASE_COUNT, HOSPITALIZED_COUNT, DEATH_COUNT, date_of_interest)
      covid_order_selectcols$dateandtime <- as.Date(as.POSIXct(covid_order_selectcols$date_of_interest, format = "%m/%d/%Y"))
      covid_ordered_filtered <- subset(covid_order_selectcols, dateandtime >= input$dates[1] & dateandtime <= input$dates[2])
      diff_dates = difftime(input$dates[2],input$dates[1], units = "days")
      diff_dates = as.numeric(diff_dates)
      covid_ordered_filtered$dateandtime <- as.POSIXct(covid_ordered_filtered$date_of_interest,format = "%m/%d/%Y")
      
      daterange=c(as.POSIXlt(min(covid_ordered_filtered$dateandtime)), as.POSIXlt(max(covid_ordered_filtered$dateandtime)))
      
      if (diff_dates > 31) {
        
        plot(covid_ordered_filtered$dateandtime, covid_ordered_filtered$HOSPITALIZED_COUNT, 
             type = "l", col = "yellow", xaxt= "n", xlab = "",ylab = "Hospitalizations", main = "Hospitalizations")
        points(covid_ordered_filtered$dateandtime, covid_ordered_filtered$HOSPITALIZED_COUNT, 
             type = "p", col = "yellow", xaxt= "n", xlab = "")
        axis.POSIXct(1, at=seq(daterange[1], daterange[2], by = "1 month"), format="%m-%d-%Y",las = 2)
      }
      
      if (diff_dates <= 31) {
        plot(covid_ordered_filtered$dateandtime, covid_ordered_filtered$HOSPITALIZED_COUNT, 
              type = "l", col = "yellow", xaxt= "n", xlab = "",ylab = "Hospitalizations", main = "Hospitalizations")
        points(covid_ordered_filtered$dateandtime, covid_ordered_filtered$HOSPITALIZED_COUNT, 
               type = "p", col = "yellow", xaxt= "n", xlab = "")
        
        
        axis.POSIXct(1, at=seq(daterange[1], daterange[2], by = "1 day"), format="%m-%d-%Y",las = 2)
      }
      
    })
    
    output$plot3=renderPlot({
      covid_ordered <- covid[order(as.Date(covid$date_of_interest, format = "%m/%d/%Y")),]
      covid_order_selectcols<- select(covid_ordered, CASE_COUNT, HOSPITALIZED_COUNT, DEATH_COUNT, date_of_interest)
      covid_order_selectcols$dateandtime <- as.Date(as.POSIXct(covid_order_selectcols$date_of_interest, format = "%m/%d/%Y"))
      covid_ordered_filtered <- subset(covid_order_selectcols, dateandtime >= input$dates[1] & dateandtime <= input$dates[2])
      diff_dates = difftime(input$dates[2],input$dates[1], units = "days")
      diff_dates = as.numeric(diff_dates)
      covid_ordered_filtered$dateandtime <- as.POSIXct(covid_ordered_filtered$date_of_interest,format = "%m/%d/%Y")
      
      daterange=c(as.POSIXlt(min(covid_ordered_filtered$dateandtime)), as.POSIXlt(max(covid_ordered_filtered$dateandtime)))
      
      if (diff_dates > 31) {
        
        plot(covid_ordered_filtered$dateandtime, covid_ordered_filtered$DEATH_COUNT, 
             type = "l", col = "red", xaxt= "n", xlab = "", ylab = "Deaths", main = "Deaths")
        points(covid_ordered_filtered$dateandtime, covid_ordered_filtered$DEATH_COUNT, 
               type = "p", col = "red", xaxt= "n", xlab = "")
        axis.POSIXct(1, at=seq(daterange[1], daterange[2], by = "1 month"), format="%m-%d-%Y",las = 2)
      }
      
      if (diff_dates <= 31) {
        plot(covid_ordered_filtered$dateandtime, covid_ordered_filtered$DEATH_COUNT, 
             type = "l", col = "red", xaxt= "n", xlab = "", ylab = "Deaths", main = "Deaths")
        points(covid_ordered_filtered$dateandtime, covid_ordered_filtered$DEATH_COUNT, 
               type = "p", col = "red", xaxt= "n", xlab = "")
        
        
        axis.POSIXct(1, at=seq(daterange[1], daterange[2], by = "1 day"), format="%m-%d-%Y",las = 2)
    
      }
    
    
    
  })

}

#### ui.R 
library(shiny)

ui <- shinyUI(fluidPage(
  
  dateRangeInput("dates", label = h3("Choose a Date Range:")),
  
  hr(),
  fluidRow(column(4, verbatimTextOutput("value"))), 
  
  mainPanel(
    splitLayout(cellWidths = c("50%", "50%", "50%"),plotOutput("plot1"), plotOutput("plot2"), 
                plotOutput("plot3"))
  ))
  
)

shinyApp(ui = ui, server = server)