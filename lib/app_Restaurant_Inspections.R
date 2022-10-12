

library(shiny)
library(dplyr)
library(leaflet)

data = read.csv("/Users/NickBarx/Documents/Education/Columbia/Courses/Applied_Data_Science/PS_2/ADS_Project2/Restaurant_Inspections.csv")
data$date_obj = as.Date(data$INSPECTION.DATE, format="%m/%d/%Y")
data$year = as.integer(format(data$date_obj, format="%Y"))
data$month = as.integer(format(data$date_obj, format="%m"))

categories = c("BORO", "CUISINE.DESCRIPTION", "ACTION", "VIOLATION.CODE", "CRITICAL.FLAG")
boroughs = c("Brooklyn", "Queens", "Bronx", "Manhattan", "Staten Island", "All")
cuisines = c(unique(data$CUISINE.DESCRIPTION), "All")


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
            
            conditionalPanel(condition="input.tabselected==3",
                             sliderInput("year_map",
                                         "Year of Grade",
                                         min = as.integer(min(data$year)),
                                         max = as.integer(max(data$year)),
                                         value = as.integer(median(data$year)),
                                         step = 1)
            ),
            
            conditionalPanel(condition="input.tabselected==3",
                             selectInput('boro_select', 'Borough', boroughs)
            ),
            
            conditionalPanel(condition="input.tabselected==3",
                             selectInput('cuisine_select', 'Cuisine', cuisines)
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
            tabPanel("Map", value=3, leafletOutput("map_plot")),
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
    
    output$map_plot <- renderLeaflet({
    
          leaflet() %>%
                addTiles() %>% 
                setView(-73.9, 40.7, zoom = 10)
          })
    
    observe({
      
      input$tabselected
      temp_data = data %>% filter(year == input$year_map)
      
      if (input$boro_select != "All") {
        temp_data = temp_data %>% filter(BORO == input$boro_select)
      }
      
      if (input$cuisine_select != "All") {
        temp_data = temp_data %>% filter(CUISINE.DESCRIPTION == input$cuisine_select)
      }
      
      map_plot <- leafletProxy('map_plot', data = temp_data) %>%
        clearMarkers() %>% 
        addCircleMarkers(lng = ~Longitude, lat = ~Latitude, fillColor = "#24D238", weight = 1, radius = 4)

    })
          
}

# Run the application 
shinyApp(ui = ui, server = server)
