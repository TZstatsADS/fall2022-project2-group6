

library(shiny)
library(dplyr)

data = read.csv("/Users/NickBarx/Documents/Education/Columbia/Courses/Applied_Data_Science/PS_2/ADS_Project2/Restaurant_Inspections.csv")
data$date_obj = as.Date(data$INSPECTION.DATE, format="%m/%d/%Y")
data$year = as.integer(format(data$date_obj, format="%Y"))
data$month = as.integer(format(data$date_obj, format="%m"))

categories = c("BORO", "CUISINE.DESCRIPTION", "ACTION", "VIOLATION.CODE", "CRITICAL.FLAG")

data = data %>% filter(year > 1900)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Exploration of How COVID-19 Changed Our Daily Lives: Restaurant Inspections"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
            conditionalPanel(condition="input.tabselected==1",
                             sliderInput("year",
                                         "Year of Grade",
                                         min = as.integer(min(data$year)),
                                         max = as.integer(max(data$year)),
                                         value = as.integer(median(data$year)),
                                         step = 1)
            ),
            
            conditionalPanel(condition="input.tabselected==2",
                             selectInput('cat', 'Category', categories)
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Calendar Year", value=1, plotOutput("barPlot_DATA")),
            tabPanel("Categorical", value=2, plotOutput("barPlot_CAT")),
            id = "tabselected"
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$barPlot_DATA <- renderPlot({
      temp_data = data %>% filter(year == input$year)
      results = temp_data %>% count(month)
      # draw the histogram with the specified number of bins
      barplot( height = results[,2], names = results[,1],
           xlab = 'Month', ylab = 'Count of Inspections',
           main = 'Monthly Inspections by Calendar Year')
    }) 
    
    output$barPlot_CAT <- renderPlot({
      pivot = input$cat
      results = data %>% group_by(!!!rlang::syms(input$cat)) %>%
                summarise( mean(SCORE, na.rm=TRUE))
      
      results = as.data.frame(results)
      labels = unique(data[[input$cat]])
      # draw the histogram with the specified number of bins
      barplot( height = c(results[,2]), names=c(results[,1]),
               xlab = 'Category', ylab = 'Average Score',
               main = 'Average Category Inspection Score')
    }) 
    
}

# Run the application 
shinyApp(ui = ui, server = server)
