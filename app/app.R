library("shiny")
library("shinydashboard")
library("dplyr")
library("ggplot2")

Air_Quality <- read.csv("Air_Quality.csv")
Air_Quality_ordered <- Air_Quality[order(Air_Quality$Geo.Type.Name,Air_Quality$Geo.Place.Name, Air_Quality$Name),]
Air_Quality_name_subset <- Air_Quality_ordered[which(Air_Quality_ordered$Name %in% c("Fine Particulate Matter (PM2.5)", "Ozone (O3)", "Nitrogen Dioxide (NO2)")),]

Neighborhood_Type_choices <- Air_Quality_name_subset[c("Geo.Type.Name", "Geo.Place.Name")]
Neighborhood_Type_choices <- Neighborhood_Type_choices[!duplicated(Neighborhood_Type_choices$Geo.Place.Name),]
Neighborhood_choices <- c("CD", "Borough", "Citywide", "UHF34", "UHF42")

Air_Quality_Type_choices <- Air_Quality_name_subset[c("Geo.Place.Name","Name")]
Air_Quality_Type_choices_sorted <- Air_Quality_Type_choices %>% arrange(Air_Quality_Type_choices$Geo.Place.Name) %>% group_by(Geo.Place.Name, Name) %>% slice(1L)

Air_Quality_subset <- Air_Quality_name_subset[c("Geo.Type.Name", "Geo.Place.Name", "Name", "Start_Date", "Data.Value")]
Air_Quality_subset_ordered <- Air_Quality_subset[order(Air_Quality_subset$Geo.Type.Name, Air_Quality_subset$Geo.Place.Name,
                                                       Air_Quality_subset$Name, as.Date(Air_Quality_subset$Start_Date, format = "%m/%d/%Y")),]






Air_Quality_subset_ordered$dateandtime <- as.POSIXct(Air_Quality_subset_ordered$Start_Date, format = "%m/%d/%Y")
Air_Quality_subset_ordered$years <- format(Air_Quality_subset_ordered$dateandtime, format = "%Y")


# Define UI for application that draws a histogram
ui <- fluidPage( 
    
    dashboardPage(
        dashboardHeader(),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Air Quality Levels in Neighborhood ", tabName = "AirQuality"),
                menuItem("Comparison - Two Neighborhoods", tabName = "AirQuality2")
             #   menuItem("Restaurant Inspections", tabName = "Restaurant Inspections")
               
        )
        ),
        dashboardBody(
            tabItems(
                # First tab content
                tabItem(tabName = "AirQuality",
                        fluidRow(
                        selectInput(
                            inputId = "neighborhood_type",
                            label = "Choose a Neighborhood type:",
                            choices = Neighborhood_choices,
                            selected = "CD"
                        ),
                        selectInput(
                            inputId = "neighborhood",
                            label = "Choose the Neighborhood:",
                            choices = NULL,
                            multiple = TRUE
                        )
                        ),
                        mainPanel(
                            splitLayout(cellWidths = c("50%", "50%", "50%"),plotOutput("Particles"), plotOutput("Nitrogen"), 
                                        plotOutput("Ozone"))
                        )
                ),
                
                tabItem(tabName = "AirQuality2",
                        fluidRow(
                          selectInput(
                            inputId = "years",
                            label = "Choose a  Year:",
                            choices = unique(Air_Quality_subset_ordered$years),
                            selected = "2008"
                          )
                        ),
                        fluidRow (
                          selectInput(
                            inputId = "neighborhood_type_2",
                            label = "Choose a Neighborhood type:",
                            choices = Neighborhood_choices,
                            selected = "CD"
                          ),
                          
                          selectInput(
                            inputId = "neighborhood_2",
                            label = "Choose the First Neighborhood:",
                            choices = NULL,
                            multiple = TRUE
                          )
                        ),
                        
                        fluidRow(
                          selectInput(
                            inputId = "neighborhood_type2_1",
                            label = "Choose a Neighborhood type:",
                            choices = Neighborhood_choices,
                            selected = "CD"
                          ),
                          
                          selectInput(
                            inputId = "neighborhood2_1",
                            label = "Choose the Second Neighborhood:",
                            choices = NULL,
                            multiple = TRUE
                          )
                          
                        ),
                        
                        mainPanel(
                          plotOutput("plot1")
                        )
                    )
              )
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
    observeEvent(input$neighborhood_type,
                 {
                     # Use this function to update the choices for the user.
                     # First argument is session, next the input to update,
                     # and third the new choices. Here, I'm filtering the
                     # previously made data.frame to based on the series column,
                     # and returning the choices column. 
                     # `drop=TRUE` makes it explicit that I want a vector returned.
                     updateSelectInput(session, input = "neighborhood",
                                       choices = Neighborhood_Type_choices[Neighborhood_Type_choices$Geo.Type.Name %in% input$neighborhood_type,
                                                                           "Geo.Place.Name", drop = TRUE])
                 })
    
    
    
    neighbor_airqual_data <- reactive({
        data <- Air_Quality_subset_ordered[(Air_Quality_subset_ordered$Geo.Type.Name == input$neighborhood_type &
                                                Air_Quality_subset_ordered$Geo.Place.Name == input$neighborhood),]
        
        data_1 <- aggregate(data$Data.Value, list(data$Start_Date, data$Geo.Type.Name, data$Geo.Place.Name, data$Name), FUN = mean)
        data_2 <- data_1[order(data_1$Group.4, as.Date(data_1$Group.1, format = "%m/%d/%Y")),]
        
        
        return(data_2)
        
        
    })
    
    
    
    output$Particles <- renderPlot({
        data = neighbor_airqual_data()
        data_particles <- data[(data$Group.4 == "Fine Particulate Matter (PM2.5)"),]
        
        data_particles$dateandtime <- as.POSIXct(data_particles$Group.1, format = "%m/%d/%Y")
        daterange=c(as.POSIXlt(min(data_particles$dateandtime)), as.POSIXlt(max(data_particles$dateandtime)))
        plot(data_particles$dateandtime, data_particles$x, type = "l", col = "blue" , xaxt= "n", xlab = "", ylab = "mcg per cubic meter", ylim = c(0,55), main = "Fine Particulate Matter (PM2.5)")
        points(data_particles$dateandtime, data_particles$x, 
               type = "p", xaxt= "n", 
               col = ifelse((data_particles$dateandtime == as.POSIXct("2020-01-01") |
                              data_particles$dateandtime == as.POSIXct("2020-06-01")), 
                            "red", "blue"), xlab = "")
        
        axis.POSIXct(1, at=seq(daterange[1], daterange[2], by = "6 months"), format="%m-%d-%Y",las = 2)
    })
    
    output$Ozone <- renderPlot({
        data = neighbor_airqual_data()
        data_ozone <- data[(data$Group.4 == "Ozone (O3)"),]
        data_ozone$dateandtime <- as.POSIXct(data_ozone$Group.1, format = "%m/%d/%Y")
        daterange=c(as.POSIXlt(min(data_ozone$dateandtime)), as.POSIXlt(max(data_ozone$dateandtime)))
        plot(data_ozone$dateandtime, data_ozone$x, type = "l", col = "blue" , xaxt= "n", xlab = "", ylab = "ppb", ylim = c(0,55), main = "Ozone (O3)")
        points(data_ozone$dateandtime, data_ozone$x, 
               type = "p", xaxt= "n", 
               col = ifelse((data_ozone$dateandtime == as.POSIXct("2020-01-01") |
                               data_ozone$dateandtime == as.POSIXct("2020-06-01")), 
                            "red", "blue"), xlab = "")
        
        
        axis.POSIXct(1, at=seq(daterange[1], daterange[2], by = "6 months"), format="%m-%d-%Y",las = 2)
        
        
    })
    
    output$Nitrogen <- renderPlot({
        data = neighbor_airqual_data()
        data_nitrogen <- data[(data$Group.4 == "Nitrogen Dioxide (NO2)"),]
        
        data_nitrogen$dateandtime <- as.POSIXct(data_nitrogen$Group.1, format = "%m/%d/%Y")
        daterange=c(as.POSIXlt(min(data_nitrogen$dateandtime)), as.POSIXlt(max(data_nitrogen$dateandtime)))
        plot(data_nitrogen$dateandtime, data_nitrogen$x, type = "l", col = "blue" , xaxt= "n", xlab = "", ylab = "ppb", ylim = c(0,55), main = "Nitrogen Dioxide (NO2)")
        points(data_nitrogen$dateandtime, data_nitrogen$x, 
               type = "p", xaxt= "n", 
               col = ifelse((data_nitrogen$dateandtime == as.POSIXct("2020-01-01") |
                               data_nitrogen$dateandtime == as.POSIXct("2020-06-01")), 
                            "red", "blue"), xlab = "")
       
        axis.POSIXct(1, at=seq(daterange[1], daterange[2], by = "6 months"), format="%m-%d-%Y",las = 2)
        
    })
    
    observeEvent(input$neighborhood_type_2,
                 {
                   # Use this function to update the choices for the user.
                   # First argument is session, next the input to update,
                   # and third the new choices. Here, I'm filtering the
                   # previously made data.frame to based on the series column,
                   # and returning the choices column. 
                   # `drop=TRUE` makes it explicit that I want a vector returned.
                   updateSelectInput(session, input = "neighborhood_2",
                                     choices = Neighborhood_Type_choices[Neighborhood_Type_choices$Geo.Type.Name %in% input$neighborhood_type_2,
                                                                         "Geo.Place.Name", drop = TRUE])
                 })
    
    observeEvent(input$neighborhood_type2_1,
                 {
                   # Use this function to update the choices for the user.
                   # First argument is session, next the input to update,
                   # and third the new choices. Here, I'm filtering the
                   # previously made data.frame to based on the series column,
                   # and returning the choices column. 
                   # `drop=TRUE` makes it explicit that I want a vector returned.
                   updateSelectInput(session, input = "neighborhood2_1",
                                     choices = Neighborhood_Type_choices[Neighborhood_Type_choices$Geo.Type.Name %in% input$neighborhood_type2_1,
                                                                         "Geo.Place.Name", drop = TRUE])
                 })
    
    
    
    neighbor_airqual_data1 <- reactive({
      
      data <- aggregate(Air_Quality_subset_ordered$Data.Value, list(Air_Quality_subset_ordered$Start_Date, Air_Quality_subset_ordered$Geo.Type.Name, 
                                                                    Air_Quality_subset_ordered$Geo.Place.Name, Air_Quality_subset_ordered$Name, 
                                                                    Air_Quality_subset_ordered$years), FUN = mean)
      data_2 <- data[order(data$Group.4, as.Date(data$Group.1, format = "%m/%d/%Y")),]
      
      data_3 <- with(data_2, data_2[((data_2$Group.5 == input$years) & (data_2$Group.2 == input$neighborhood_type_2)
                                     & (data_2$Group.3 == input$neighborhood_2)) |
                                      ((data_2$Group.5 == input$years) & (data_2$Group.2 == input$neighborhood_type2_1)
                                       & (data_2$Group.3 == input$neighborhood2_1)),])
      
      
      return(data_3)
    })
    
    
    
    output$plot1 <- renderPlot({
      data_3 <- neighbor_airqual_data1()
      plot1 <- ggplot(data_3, mapping = aes(x = data_3$Group.4, y = data_3$x, fill = data_3$Group.3)) +
        geom_bar(stat = "identity", position = "dodge") +
        xlab ("Air Quality Measures") + ylab("Frequency") + 
        scale_fill_discrete(name="Neighborhoods")
      
      return(plot1)
      
      
    })

  
}

# Run the application 
shinyApp(ui = ui, server = server)
