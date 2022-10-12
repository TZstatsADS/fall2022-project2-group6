library("shiny")
library("shinydashboard")
library("dplyr")
library("ggplot2")
library("leaflet")
library("reshape2")
library("tidytext")
library("tidyverse")
library("tm")

Air_Quality <- read.csv("Air_Quality.csv")
Air_Quality$Geo.Type.Name[Air_Quality$Geo.Type.Name == 'CD'] <- 'Community District'
Air_Quality$Geo.Type.Name[Air_Quality$Geo.Type.Name == 'UHF42'] <- 'United Hospital Fund Neighborhoods [42]'
Air_Quality$Geo.Type.Name[Air_Quality$Geo.Type.Name == 'UHF34'] <- 'United Hospital Fund Neighborhoods [34]'
Air_Quality_ordered <- Air_Quality[order(Air_Quality$Geo.Type.Name,Air_Quality$Geo.Place.Name, Air_Quality$Name),]
Air_Quality_name_subset <- Air_Quality_ordered[which(Air_Quality_ordered$Name %in% c("Fine Particulate Matter (PM2.5)", "Ozone (O3)", "Nitrogen Dioxide (NO2)")),]
Air_Quality_name_subset_UHF34 <- Air_Quality_name_subset[!Air_Quality_name_subset$Geo.Type.Name=='United Hospital Fund Neighborhoods [34]',]
UHF34_subset <- Air_Quality_name_subset[Air_Quality_name_subset$Geo.Type.Name == "United Hospital Fund Neighborhoods [34]",] 

UHF34_subset_filtered <-UHF34_subset[(UHF34_subset$Geo.Place.Name== "Bayside Little Neck-Fresh Meadows" | 
                                          UHF34_subset$Geo.Place.Name=="South Bronx" |
                                          UHF34_subset$Geo.Place.Name=="Upper East Side-Gramercy" |
                                          UHF34_subset$Geo.Place.Name=="Chelsea-Village" |
                                          UHF34_subset$Geo.Place.Name=="Southern SI" |
                                          UHF34_subset$Geo.Place.Name=="Northern SI" |
                                          UHF34_subset$Geo.Place.Name=="Union Square-Lower Manhattan" 
),]

Air_Quality_name_subset <- rbind(Air_Quality_name_subset_UHF34, UHF34_subset_filtered)
Neighborhood_Type_choices <- Air_Quality_name_subset[c("Geo.Type.Name", "Geo.Place.Name")]
Neighborhood_Type_choices <- Neighborhood_Type_choices[!duplicated(Neighborhood_Type_choices$Geo.Place.Name),]
Neighborhood_choices <- c("Community District", "Borough", "United Hospital Fund Neighborhoods [42]", "United Hospital Fund Neighborhoods [34]","Citywide")

Air_Quality_Type_choices <- Air_Quality_name_subset[c("Geo.Place.Name","Name")]
Air_Quality_Type_choices_sorted <- Air_Quality_Type_choices %>% arrange(Air_Quality_Type_choices$Geo.Place.Name) %>% group_by(Geo.Place.Name, Name) %>% slice(1L)

Air_Quality_subset <- Air_Quality_name_subset[c("Geo.Type.Name", "Geo.Place.Name", "Name", "Start_Date", "Data.Value")]
Air_Quality_subset_ordered <- Air_Quality_subset[order(Air_Quality_subset$Geo.Type.Name, Air_Quality_subset$Geo.Place.Name,
                                                       Air_Quality_subset$Name, as.Date(Air_Quality_subset$Start_Date, format = "%m/%d/%Y")),]
Air_Quality_subset_ordered$dateandtime <- as.POSIXct(Air_Quality_subset_ordered$Start_Date, format = "%m/%d/%Y")
Air_Quality_subset_ordered$years <- format(Air_Quality_subset_ordered$dateandtime, format = "%Y")



beach_dataset <- read.csv("beach_2021.csv")
beach_dataset$Date <- strftime(beach_dataset$Sample.Date,"%m/%d/%Y")
beach_dataset$dateandtime <- as.POSIXct(beach_dataset$Date,format = "%m/%d/%Y")
beach_dataset_ordered <- beach_dataset[order(beach_dataset$Beach.Name, beach_dataset$Sample.Location,as.Date(beach_dataset$dateandtime, format = "%m/%d/%Y")),]
beach_choices <- unique(beach_dataset_ordered$Beach.Name)
beach_dataset_ordered$years <- format(beach_dataset_ordered$dateandtime, format = "%Y")


covid <- read.csv("COVID-19_Daily_Counts_of_Cases__Hospitalizations__and_Deaths.csv")


restaurant_inspections <- read.csv("DOHMH_New_York_City_Restaurant_Inspection_Results.csv")
restaurant_inspections_filtered <- restaurant_inspections[!(is.na(restaurant_inspections$VIOLATION.DESCRIPTION) | restaurant_inspections$VIOLATION.DESCRIPTION==""), ]

restaurant_inspections_filtered_1 <- restaurant_inspections_filtered[!(is.na(restaurant_inspections_filtered$Latitude) | restaurant_inspections_filtered$Latitude=="" | 
                                                                           is.na(restaurant_inspections_filtered$Longitude) | restaurant_inspections_filtered$Longitude=="") , ]
restaurant_inspections_filtered_1$dateandtime <- as.POSIXct(restaurant_inspections_filtered_1$INSPECTION.DATE, format = "%m/%d/%Y")
restaurant_inspections_filtered_1$years <- format(restaurant_inspections_filtered_1$dateandtime, format = "%Y")
restaurant_inspections_filtered_2 <- restaurant_inspections_filtered_1[which(restaurant_inspections_filtered_1$years %in% c("2018", "2019", "2020",
                                                                                                                            "2021", "2022", "2017",
                                                                                                                            "2016")),]


restaurant_inspections_filtered_3 <- restaurant_inspections_filtered_2[order(restaurant_inspections_filtered_2$DBA, 
                                                                             as.Date(restaurant_inspections_filtered_2$INSPECTION.DATE, format = "%m/%d/%Y")),]

restaurant_inspections_filtered_3$criticalviolations <- ifelse(restaurant_inspections_filtered_3$CRITICAL.FLAG == "Critical", 1, 0)
restaurant_inspections_filtered_3$noncriticalviolations <- ifelse(restaurant_inspections_filtered_3$CRITICAL.FLAG == "Not Critical", 1, 0)


restaurant_inspections_filtered_3$ACTION <- ifelse(restaurant_inspections_filtered_3$ACTION == "Violations were cited in the following area(s).", 
                                                   "Violations", ifelse(restaurant_inspections_filtered_3$ACTION == "No violations were recorded at the time of this inspection.",
                                                                        "No violations", ifelse(restaurant_inspections_filtered_3$ACTION == "Establishment Closed by DOHMH. Violations were cited in the following area(s) and those requiring immediate action were addressed.",
                                                                                                "Closed", ifelse(restaurant_inspections_filtered_3$ACTION == "Establishment re-closed by DOHMH.", "Re-closed",
                                                                                                                 "Re-opened")
                                                                        )
                                                   )
)
# Define UI for application that draws a histogram
ui <- fluidPage( 
    
    dashboardPage(
        skin = "black",
        dashboardHeader(),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Introduction", tabName = "Introduction"),
                menuItem("COVID", tabName = "covid"),
                menuItem("Restaurant Inspections", tabName = "restaurant"),
                menuItem("Beach Water Quality", tabName = "beachQuality2"),
                menuItem("Air Quality Levels in Neighborhood ", tabName = "AirQuality"),
                menuItem("Air Quality Levels in 2 Neighborhoods", tabName = "AirQuality2")
            )
        ),
        dashboardBody(
            tabItems(
                # First tab content
                
                tabItem(tabName = "Introduction", fluidPage(
                    fluidRow(box(width = 15, title = "Introduction", status = "primary",
                                 solidHeader = TRUE, h3("Covid-19 and the Impact on Air Quality, Beach Water Quality and Restaurants"),
                                 h4("By Saumya Pandey, Ying Gao, and Nicholas Baker"),
                                 h5(strong("Objective:"), "To see the general trends and insights, and also some insights and trends pre-COVID and after COVID on topics like
                                    air quality, beach water quality and restaurant inspections that fell under the Department of Mental Health and Hygiene."),
                                 h5("One commonality in all of these areas are that they are determined/impacted by people."),
                                 h5("Regarding", strong("Air Quality"), ", there are three types of Air Pollutants that we looked at:"),
                                 h5("Fine Particulate Matter: Fine particles primarily come from cars, trucks, buses, industrial power plants, etc."),
                                 h5("Nitrogen Dioxide: Comes primarily from the engines of automobiles and factories."),
                                 h5("Ozone: If pollutants emitted from automobiles, power plants, chemical plants, etc. chemically react in the sunlight, 
                                    it could lead to an increase in Ozone."),
                                 h5(strong("Hypothesis:"), "Since people stayed at home during COVID, during that phase, there could have been significant improvements in Air Quality."),
                                
                                 h5("Regarding", strong("Beach Water Quality"), ", Beach quality is tested and measured by Enterococci sample results collected from 
                                    different beaches in New York City. The increasing Enterococci level indicates that the beach quality is getting worse. 
                                    Enterrocci is determined by pets, humans, wildlife, etc."),
                                 h5(strong("Hypothesis:"), "Since people stayed at home during COVID, during that phase, there could have been significant improvements in Beach Water Quality."),
                                
                                 h5("Regarding", strong("Restaurant Inspections"), ", those are done to see if 
                                 restaurants are in compliance with the health and the safety requirements which entirely depends on people/employees. 
                                 Variables like employee hygiene, the areas being clean and free of vermin, safe food handling, etc. are taken into account. 
                                 Not being in compliance with them can lead to severe problems like customers developing food borne illness and restaurants, themselves,
                                 facing critical and non critical violations 
                                 which can lead to the restaurant shutting down."),
                                 h5(strong("Hypothesis:"), "Due to restaurant closures during COVID, travel restrictions,  
                                 staying at home, changes to daily life and hygiene behaviors like increased hand washing, etc.
                                 this could have lead to a decrease in the violations after COVID.")
                                 )),
                    fluidRow(box(width = 15, title = "What the App Provides?", status = "primary", solidHeader=TRUE,
                                 h5("The application is divided into 5 separate tabs"),
                                 tags$div(tags$ul(
                                     tags$li("The", strong("first"), "tab: Introduction"),
                                     tags$li("The", strong("second"), "tab: The detailed ZIP code map shows the extent of Covid 19 outbreak in NYC. It provided key information including: confirmed cases, infection rate, number of business that are closed in the neighborhood"),
                                     tags$li("The", strong("third and fourth"), "tab: stats on recently opened/ closed business during Covid 19, tracked separately for different industries"),
                                     tags$li("The", strong("fifth"),"tab: Appendix and data sources")
                                     
                                 ))
                                 
                                 
                                 
                                 )),
                    
                    fluidRow(box(width = 15, title = "How to Use The App", status = "primary",
                                 solidHeader = TRUE,
                                 h5("The application is divided into 5 separate tabs"),
                                 tags$div(tags$ul(
                                     tags$li("The", strong("first"), "tab: Introduction"),
                                     tags$li("The", strong("second"), "tab: The detailed ZIP code map shows the extent of Covid 19 outbreak in NYC. It provided key information including: confirmed cases, infection rate, number of business that are closed in the neighborhood"),
                                     tags$li("The", strong("third and fourth"), "tab: stats on recently opened/ closed business during Covid 19, tracked separately for different industries"),
                                     tags$li("The", strong("fifth"),"tab: Appendix and data sources")
                                     
                                 ))
                    ))
                )), 
                # end of home 
                
                
                tabItem(tabName = "restaurant", fluidPage(
                    fluidRow(
                        column(width = 9,
                               box(width = NULL, solidHeader = TRUE,
                                   leafletOutput("map", height = 500)
                               ),
                               box(width = NULL, plotOutput("criticalmonths", height = 200)),
                               box(width = NULL, plotOutput("borough", height = 200)),
                               #         box(width = NULL, plotOutput("noncriticalviolations", height = 200)),
                               box(width = NULL, plotOutput("criticalviolations", height = 200))
                               
                        ),
                        column(width = 3,
                               box(width = NULL,  
                                   selectInput(
                                       inputId = "years_1",
                                       label = "Choose a Year:",
                                       choices = c("2016", "2017",
                                                   "2018", "2019",
                                                   "2020", "2021",
                                                   "2022"),
                                       selected = "2019"
                                   )
                                   
                               ),
                               box(width = NULL, plotOutput("violations", height = 200)),
                               box(width = NULL, plotOutput("action", height = 200))
                        )
                        
                    )
                )
                ),
                #end of restaurant inspections
                
                tabItem(tabName = "beachQuality2", fluidPage(
                    
                    fluidRow(
                        column(width = 3,
                               box(width = NULL,  
                                   selectInput(
                                       inputId = "beach",
                                       label = "Choose a Beach type:",
                                       choices = beach_choices,
                                       selected = "AMERICAN TURNERS"
                                   )
                                   
                               )
                               
                        )
                    ),
                    fluidRow(
                        column(width = 12,
                               box(width = NULL, plotOutput("center", height = 300)),
                               box(width = NULL, plotOutput("right", height = 300)),
                               box(width = NULL, plotOutput("left", height = 300))
                        )
                    )
                )
                
                ),
                tabItem(tabName = "covid", fluidPage (
                    fluidRow(
                        column(width = 3,
                               box(width = NULL,  
                                   fluidRow(
                                       dateRangeInput("dates", label = h3("Choose a Date Range:"), start = "2022-09-25"),
                                       
                                       #  hr(),
                                       # fluidRow(verbatimTextOutput("value"))
                                       
                                   )
                                   
                               )
                        )
                    ),
                    fluidRow(
                        column(width = 12,
                               box(width = NULL, plotOutput("cases", height = 300)),
                               box(width = NULL, plotOutput("hospitalizations", height = 300)),
                               box(width = NULL, plotOutput("deaths", height = 300))
                        )
                    )
                )
                ),
                
                
                
                tabItem(tabName = "AirQuality",
                        fluidRow(
                            selectInput(
                                inputId = "neighborhood_type",
                                label = "Choose a Neighborhood type:",
                                choices = Neighborhood_choices,
                                selected = "Borough"
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
                            div(style="display: inline-block;vertical-align:top; width: 150px;", selectInput(
                                inputId = "years",
                                label = "Choose a  Year:",
                                choices = unique(Air_Quality_subset_ordered$years),
                                selected = "2019"
                            )
                            )
                        ),
                        fluidRow (
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                selectInput(
                                    inputId = "neighborhood_type_2",
                                    label = "Choose a Neighborhood type:",
                                    choices = Neighborhood_choices,
                                    selected = "Borough"
                                )
                            ),
                            
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                selectInput(
                                    inputId = "neighborhood_2",
                                    label = "Choose the First Neighborhood:",
                                    choices = NULL,
                                    multiple = TRUE
                                )
                            )
                        ),
                        
                        fluidRow(
                            div(style="display: inline-block;vertical-align:top; width: 150px;", selectInput(
                                inputId = "neighborhood_type2_1",
                                label = "Choose a Neighborhood type:",
                                choices = Neighborhood_choices,
                                selected = "Borough"
                            )
                            ),
                            
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                selectInput(
                                    inputId = "neighborhood2_1",
                                    label = "Choose the Second Neighborhood:",
                                    choices = NULL,
                                    multiple = TRUE
                                )
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
                     x <- input$neighborhood_type
                     
                     if(x=='Borough'){
                         updateSelectInput(session,'neighborhood',
                                           choices = Neighborhood_Type_choices[Neighborhood_Type_choices$Geo.Type.Name %in% input$neighborhood_type,
                                                                               "Geo.Place.Name", drop = TRUE],
                                           selected = 'Manhattan')
                         
                     }else if(x=='United Hospital Fund Neighborhoods [42]'){
                         updateSelectInput(session,'neighborhood',
                                           choices = Neighborhood_Type_choices[Neighborhood_Type_choices$Geo.Type.Name %in% input$neighborhood_type,
                                                                               "Geo.Place.Name", drop = TRUE],
                                           selected = 'Bayside - Little Neck')
                     }else if(x=='United Hospital Fund Neighborhoods [34]'){
                         updateSelectInput(session,'neighborhood',
                                           choices = Neighborhood_Type_choices[Neighborhood_Type_choices$Geo.Type.Name %in% input$neighborhood_type,
                                                                               "Geo.Place.Name", drop = TRUE],
                                           selected = 'Bayside Little Neck-Fresh Meadows')
                     }else if(x=='Community District'){
                         updateSelectInput(session,'neighborhood',
                                           choices = Neighborhood_Type_choices[Neighborhood_Type_choices$Geo.Type.Name %in% input$neighborhood_type,
                                                                               "Geo.Place.Name", drop = TRUE],
                                           selected = "Bay Ridge and Dyker Heights (CD10)")
                     }
                     else{
                         updateSelectInput(session,'neighborhood',
                                           choices = Neighborhood_Type_choices[Neighborhood_Type_choices$Geo.Type.Name %in% input$neighborhood_type,
                                                                               "Geo.Place.Name", drop = TRUE],
                                           selected = 'New York City')
                     }
                     
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
        plot(data_particles$dateandtime, data_particles$x, type = "l", col = "blue" , xaxt= "n", xlab = "", ylab = "mcg per cubic meter", main = "Fine Particulate Matter (PM2.5)")
        points(data_particles$dateandtime, data_particles$x, 
               type = "p", xaxt= "n", 
               col = ifelse((data_particles$dateandtime >= "2020-01-01"), 
                            "red", "blue"), xlab = "")
        
        axis.POSIXct(1, at=seq(daterange[1], daterange[2], by = "6 months"), format="%m-%d-%Y",las = 2)
    })
    
    output$Ozone <- renderPlot({
        data = neighbor_airqual_data()
        data_ozone <- data[(data$Group.4 == "Ozone (O3)"),]
        data_ozone$dateandtime <- as.POSIXct(data_ozone$Group.1, format = "%m/%d/%Y")
        daterange=c(as.POSIXlt(min(data_ozone$dateandtime)), as.POSIXlt(max(data_ozone$dateandtime)))
        plot(data_ozone$dateandtime, data_ozone$x, type = "l", col = "blue" , xaxt= "n", xlab = "", ylab = "ppb", main = "Ozone (O3)")
        points(data_ozone$dateandtime, data_ozone$x, 
               type = "p", xaxt= "n", 
               col = ifelse((data_ozone$dateandtime >= "2020-01-01"), 
                            "red", "blue"), xlab = "")
        
        
        axis.POSIXct(1, at=seq(daterange[1], daterange[2], by = "6 months"), format="%m-%d-%Y",las = 2)
        
        
    })
    
    output$Nitrogen <- renderPlot({
        data = neighbor_airqual_data()
        data_nitrogen <- data[(data$Group.4 == "Nitrogen Dioxide (NO2)"),]
        
        data_nitrogen$dateandtime <- as.POSIXct(data_nitrogen$Group.1, format = "%m/%d/%Y")
        daterange=c(as.POSIXlt(min(data_nitrogen$dateandtime)), as.POSIXlt(max(data_nitrogen$dateandtime)))
        plot(data_nitrogen$dateandtime, data_nitrogen$x, type = "l", col = "blue" , xaxt= "n", xlab = "", ylab = "ppb", main = "Nitrogen Dioxide (NO2)")
        points(data_nitrogen$dateandtime, data_nitrogen$x, 
               type = "p", xaxt= "n", 
               col = ifelse((data_nitrogen$dateandtime >="2020-01-01"), 
                            "red", "blue"), xlab = "")
        
        axis.POSIXct(1, at=seq(daterange[1], daterange[2], by = "6 months"), format="%m-%d-%Y",las = 2)
        
    })
    
    observeEvent(input$neighborhood_type_2,
                 {
                     x <- input$neighborhood_type_2
                     
                     if(x=='Borough'){
                         updateSelectInput(session,'neighborhood_2',
                                           choices = Neighborhood_Type_choices[Neighborhood_Type_choices$Geo.Type.Name %in% input$neighborhood_type_2,
                                                                               "Geo.Place.Name", drop = TRUE],
                                           selected = 'Manhattan')
                         
                     }else if(x=='United Hospital Fund Neighborhoods [42]'){
                         updateSelectInput(session,'neighborhood_2',
                                           choices = Neighborhood_Type_choices[Neighborhood_Type_choices$Geo.Type.Name %in% input$neighborhood_type_2,
                                                                               "Geo.Place.Name", drop = TRUE],
                                           selected = 'Bayside - Little Neck')
                     }else if(x=='United Hospital Fund Neighborhoods [34]'){
                         updateSelectInput(session,'neighborhood_2',
                                           choices = Neighborhood_Type_choices[Neighborhood_Type_choices$Geo.Type.Name %in% input$neighborhood_type_2,
                                                                               "Geo.Place.Name", drop = TRUE],
                                           selected = 'Bayside Little Neck-Fresh Meadows')
                     }else if(x=='Community District'){
                         updateSelectInput(session,'neighborhood_2',
                                           choices = Neighborhood_Type_choices[Neighborhood_Type_choices$Geo.Type.Name %in% input$neighborhood_type_2,
                                                                               "Geo.Place.Name", drop = TRUE],
                                           selected = "Bay Ridge and Dyker Heights (CD10)")
                     }
                     else{
                         updateSelectInput(session,'neighborhood_2',
                                           choices = Neighborhood_Type_choices[Neighborhood_Type_choices$Geo.Type.Name %in% input$neighborhood_type_2,
                                                                               "Geo.Place.Name", drop = TRUE],
                                           selected = 'New York City')
                     }
            
                 })
    
    observeEvent(input$neighborhood_type2_1,
                 {
                     x <- input$neighborhood_type2_1
                     
                     
                     if(x=='Borough'){
                         updateSelectInput(session,'neighborhood2_1',
                                           choices = Neighborhood_Type_choices[Neighborhood_Type_choices$Geo.Type.Name %in% input$neighborhood_type2_1,
                                                                               "Geo.Place.Name", drop = TRUE],
                                           selected = 'Manhattan')
                         
                     }else if(x=='United Hospital Fund Neighborhoods [42]'){
                         updateSelectInput(session,'neighborhood2_1',
                                           choices = Neighborhood_Type_choices[Neighborhood_Type_choices$Geo.Type.Name %in% input$neighborhood_type2_1,
                                                                               "Geo.Place.Name", drop = TRUE],
                                           selected = 'Bayside - Little Neck')
                     }else if(x=='United Hospital Fund Neighborhoods [34]'){
                         updateSelectInput(session,'neighborhood2_1',
                                           choices = Neighborhood_Type_choices[Neighborhood_Type_choices$Geo.Type.Name %in% input$neighborhood_type2_1,
                                                                               "Geo.Place.Name", drop = TRUE],
                                           selected = 'Bayside Little Neck-Fresh Meadows')
                     }else if(x=='Community District'){
                         updateSelectInput(session,'neighborhood2_1',
                                           choices = Neighborhood_Type_choices[Neighborhood_Type_choices$Geo.Type.Name %in% input$neighborhood_type2_1,
                                                                               "Geo.Place.Name", drop = TRUE],
                                           selected = "Bay Ridge and Dyker Heights (CD10)")
                     }
                     else{
                         updateSelectInput(session,'neighborhood2_1',
                                           choices = Neighborhood_Type_choices[Neighborhood_Type_choices$Geo.Type.Name %in% input$neighborhood_type2_1,
                                                                               "Geo.Place.Name", drop = TRUE],
                                           selected = 'New York City')
                     }
                     
                     
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
    
    right_value <- reactive ({
        right <- sum(beach_dataset_ordered[beach_dataset_ordered$Beach.Name == input$beach,]$Sample.Location=="Right")
        return(right)
    })
    
    left_value <- reactive ({
        left <- sum(beach_dataset_ordered[beach_dataset_ordered$Beach.Name == input$beach,]$Sample.Location=="Left")
        return(left)
    })
    center_value <- reactive ({
        center <- sum(beach_dataset_ordered[beach_dataset_ordered$Beach.Name == input$beach,]$Sample.Location=="Center")
        return(center)
    })
    
    
    right_data <- reactive({
        beach_dataset_filtered_right <- subset(beach_dataset_ordered, (beach_dataset_ordered$Beach.Name == input$beach & beach_dataset_ordered$Sample.Location == "Right"))
        right = right_value()
        if (right >= 20) {
            beach_table_right <- aggregate(list(Enterococci = beach_dataset_filtered_right$Enterococci.Results),
                                           list(month = cut(as.POSIXlt(beach_dataset_filtered_right$dateandtime), 
                                                            breaks = "1 month")),mean)
            
            beach_table_right$dateandtime <- as.POSIXct(beach_table_right$month, format = "%Y-%m-%d")
            return(beach_table_right)
            
        } else {
            return(beach_dataset_filtered_right)
        }
    })
    
    
    
    output$right <- renderPlot({
        data = right_data()
        right = right_value()
        if(right >20) {
            daterange=c(as.POSIXlt(min(data$dateandtime)), as.POSIXlt(max(data$dateandtime)))
            plot(data$dateandtime, data$Enterococci, type = "l", xaxt= "n", xlab = "", col = "darkgreen", main = "Right", ylab = "Enterococci")
            points(data$dateandtime, data$Enterococci,
                   col = ifelse(data$dateandtime >= "2020-01-01", 
                                "red", "darkgreen"), xlab = "")
            
            
            axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="3 months"), format="%m/%Y",las = 2)
        } else {
            daterange=c(as.POSIXlt(min(data$dateandtime)), as.POSIXlt(max(data$dateandtime)))
            plot(data$dateandtime, data$Enterococci, type = "l", xaxt= "n", xlab = "", col = "darkgreen", main = "Right", ylab = "Enterococci")
            points(data$dateandtime, data$Enterococci,  col = ifelse(data$dateandtime >= "2020-01-01", 
                                                                     "red", "darkgreen"), xlab = "")
            
            axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="1 month"), format="%m/%Y",las = 2)
        }
    })
    
    left_data <- reactive({
        beach_dataset_filtered_left <- subset(beach_dataset_ordered, (beach_dataset_ordered$Beach.Name == input$beach & beach_dataset_ordered$Sample.Location == "Left"))
        left = left_value()
        
        if(left >20) {
            
            beach_table_left <- aggregate(list(Enterococci = beach_dataset_filtered_left$Enterococci.Results),
                                          list(month = cut(as.POSIXlt(beach_dataset_filtered_left$dateandtime), 
                                                           breaks = "1 month")),mean)
            beach_table_left$dateandtime <- as.POSIXct(beach_table_left$month, format = "%Y-%m-%d")
            
            return(beach_table_left)
            
        } else {
            return(beach_dataset_filtered_left)
        }
    })
    
    
    
    output$left <- renderPlot({
        data = left_data()
        left = left_value()
        if(left > 20) {
            daterange=c(as.POSIXlt(min(data$dateandtime)), as.POSIXlt(max(data$dateandtime)))
            plot(data$dateandtime, data$Enterococci, type = "l", xaxt= "n", xlab = "", col = "blue", main = "Left", ylab = "Enterococci")
            points(data$dateandtime, data$Enterococci, col = ifelse(data$dateandtime >= "2020-01-01", 
                                                                    "red", "blue"), xlab = "")
            axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="3 months"), format="%m/%Y",las = 2)
            
        } else {
            daterange=c(as.POSIXlt(min(data$dateandtime)), as.POSIXlt(max(data$dateandtime)))
            plot(data$dateandtime, data$Enterococci, type = "l", xaxt= "n", xlab = "", col = "blue", main = "Left", ylab = "Enterococci")
            points(data$dateandtime, data$Enterococci, col = ifelse(data$dateandtime >= "2020-01-01", 
                                                                    "red", "blue"), xlab = "")
            axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="1 month"), format="%m/%Y",las = 2)
        }
        
    })
    
    center_data <- reactive({
        beach_dataset_filtered_center <- subset(beach_dataset_ordered, (beach_dataset_ordered$Beach.Name == input$beach & beach_dataset_ordered$Sample.Location == "Center"))
        center = center_value()
        if(center > 20) {
            beach_table_center <- aggregate(list(Enterococci = beach_dataset_filtered_center$Enterococci.Results),
                                            list(month = cut(as.POSIXlt(beach_dataset_filtered_center$dateandtime), 
                                                             breaks = "1 month")),mean)
            beach_table_center$dateandtime <- as.POSIXct(beach_table_center$month, format = "%Y-%m-%d")
            
            return(beach_table_center)
        } else {
            return(beach_dataset_filtered_center)
        }
    })
    
    
    
    output$center <- renderPlot({
        data = center_data()
        center = center_value()
        
        if (center > 20) {
            daterange=c(as.POSIXlt(min(data$dateandtime)), as.POSIXlt(max(data$dateandtime)))
            plot(data$dateandtime, data$Enterococci, type = "l", xaxt= "n", xlab = "", col = "black", main = "Center", ylab = "Enterococci")
            points(data$dateandtime, data$Enterococci, col = ifelse(data$dateandtime >= "2020-01-01", 
                                                                    "red", "black"), xlab = "")
            axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="3 months"), format="%m/%Y",las = 2)
        } else {
            daterange=c(as.POSIXlt(min(data$dateandtime)), as.POSIXlt(max(data$dateandtime)))
            plot(data$dateandtime, data$Enterococci, type = "l", xaxt= "n", xlab = "", col = "black", main = "Center", ylab = "Enterococci")
            points(data$dateandtime, data$Enterococci, col = ifelse(data$dateandtime >= "2020-01-01", 
                                                                    "red", "black"), xlab = "")
            axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="1 month"), format="%m/%Y",las = 2)
            
        }
        
        
    })
    
    output$cases=renderPlot({
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
                 ylab = "Cases", main = "Cases")
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
    
    
    output$hospitalizations=renderPlot({
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
    
    output$deaths=renderPlot({
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
    
    years_2 <- reactive({
        restaurant_inspections_filtered_year <- restaurant_inspections_filtered_3[restaurant_inspections_filtered_3$years == input$years_1,]
        restaurant_inspections_filtered_year$months <- format(restaurant_inspections_filtered_year$dateandtime, format = "%m-%Y")
        df1 <- aggregate(cbind(restaurant_inspections_filtered_year$criticalviolations,restaurant_inspections_filtered_year$noncriticalviolations) ~ 
                             restaurant_inspections_filtered_year$DBA, data = restaurant_inspections_filtered_year, FUN = sum, na.rm = TRUE)
        
        df1$criticalYN <- ifelse(df1$V1 > df1$V2, "Critical", "Non-Critical")
        df1$DBA <- df1$`restaurant_inspections_filtered_year$DBA`
        df1$critical <- df1$V1
        df1$noncritical <- df1$V2
        df1 <-subset(df1, select = -c(V1, V2, `restaurant_inspections_filtered_year$DBA`))
        
        
        restaurant_inspections_filtered_year <-subset(restaurant_inspections_filtered_year, select = -c(VIOLATION.CODE, CRITICAL.FLAG))
        total <- merge(df1, restaurant_inspections_filtered_year,by="DBA")
        total1 <- total %>% distinct(DBA, .keep_all = TRUE)
        
        return(total1)
        
        
    })
    
    
    
    output$map <- renderLeaflet({
        data <- years_2()
        color <- colorFactor(topo.colors(2), data$criticalYN)
        leaflet(data) %>%
            setView(lng = -73.98928, lat = 40.75042, zoom = 10) %>%
            addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(
                lng=~data$Longitude, # Longitude coordinates
                lat=~data$Latitude, # Latitude coordinates
                radius=~ifelse(data$criticalYN == "Critical", 2, 2), # Total count
                stroke=FALSE, # Circle stroke
                fillOpacity=1, # Circle Fill Opacity
                color=~color(data$criticalYN),
                popup=~paste(
                    "<b>", data$DBA , "</b><br/>",
                    "criticalYN : ", as.character(data$criticalYN), "<br/>",
                    "Borough:", as.character(data$BORO),
                    "date: ", as.character(data$dateandtime
                    )
                )
                # Popup content
            ) %>%
            addLegend(
                "bottomleft", # Legend position
                pal=color, # color palette
                values=~data$criticalYN, # legend values
                opacity = 1,
                title="Critical Violation"
            )
        
    })
    
    output$violations <- renderPlot({
        data <- years_2()
        table_violations <- table(data$criticalYN)
        data_table_violations <- data.frame(table_violations)
        plot1 <- ggplot(data_table_violations, mapping = aes(x = data_table_violations$Var1, y = data_table_violations$Freq)) +
            geom_bar(stat = "identity", position = "dodge") +
            xlab ("Violation") + ylab("Frequency") 
        return(plot1)
        
        
        
        
    })
    
    output$action <- renderPlot({
        data <- years_2()
        table_actions <- table(data$ACTION)
        data_table_actions <- data.frame(table_actions)
        data_table_actions <- subset(data_table_actions, data_table_actions$Var1!="Violations")
        plot2 <- ggplot(data_table_actions, mapping = aes(x = data_table_actions$Var1, y = data_table_actions$Freq)) +
            geom_bar(stat = "identity", position = "dodge") +
            xlab ("Action") + ylab("Frequency") 
        return(plot2)
        
    })
    
    
    output$criticalmonths <- renderPlot({
        total1 <- years_2()
        critical_months <- data.frame(total1$months, total1$criticalYN)
        critical_months_table <- table(critical_months)
        df_critical_months <- data.frame(critical_months_table)
        plot2 <- ggplot(df_critical_months, aes(x = df_critical_months$total1.months, y = df_critical_months$Freq)) +
            geom_bar(aes(fill = df_critical_months$total1.criticalYN),stat = "identity", position = "dodge") + 
            xlab("Months") + ylab("Frequency") + labs(fill='Critical Violation') 

        return(plot2)
        
        
        
        
    })
    
    
    output$borough <- renderPlot({
        total1 <- years_2()
        critical_months <- data.frame(total1$BORO, total1$criticalYN)
        critical_months_table <- table(critical_months)
        df_critical_months <- data.frame(critical_months_table)
        plot2 <- ggplot(df_critical_months, aes(x = df_critical_months$total1.BORO, y = df_critical_months$Freq)) + 
            geom_bar(aes(fill = df_critical_months$total1.criticalYN),stat = "identity", position = "dodge") + 
            xlab("Borough") + ylab("Frequency") + labs(fill='Critical Violation') 
            
        
        return(plot2)
    })
    
    output$criticalviolations <- renderPlot({
        restaurant_inspections_year <- restaurant_inspections_filtered_3[restaurant_inspections_filtered_3$years == input$years_1,]
        restaurant_inspections_year_critical <- Corpus(VectorSource(restaurant_inspections_year$VIOLATION.DESCRIPTION))
        restaurant_inspections_year_critical <- tm_map(restaurant_inspections_year_critical, content_transformer(tolower))
        restaurant_inspections_year_critical <- tm_map(restaurant_inspections_year_critical, removeNumbers)
        restaurant_inspections_year_critical <- tm_map(restaurant_inspections_year_critical, removeWords, stopwords("english"))
        restaurant_inspections_year_critical <- tm_map(restaurant_inspections_year_critical, removeWords, c("used", "use", "unit", "underneath","raised", "maintained", "item", 
                                                                                                            "allow", "spaced", "preparation", "areas", "unacceptable", "surface",
                                                                                                            "sides", "sealed", "provided", "properly", "proof", "present", "held",
                                                                                                            "exist", "attracting", "allowing", "contact", "improperly",
                                                                                                            "constructed", "material", "equipment", "accessibility", "house", "include", "movable",
                                                                                                            "facility", "premises", "conditions", "conducive", "reduced","service", "live",
                                                                                                            "may", "required", "cleaning", "following", "activity", "evidence", "occurred")) 
        restaurant_inspections_year_critical <- tm_map(restaurant_inspections_year_critical, removePunctuation)
        restaurant_inspections_year_critical <- tm_map(restaurant_inspections_year_critical, stripWhitespace)
        restaurant_inspections_year_critical_tdm <- TermDocumentMatrix(restaurant_inspections_year_critical)
        restaurant_inspections_year_critical_tdm_matrix <- as.matrix(restaurant_inspections_year_critical_tdm)
        restaurant_inspections_year_critical_tdm_matrix_rowsums <- rowSums(restaurant_inspections_year_critical_tdm_matrix)
        restaurant_inspections_year_critical_tdm_matrix_sorted <- sort(restaurant_inspections_year_critical_tdm_matrix_rowsums,decreasing=TRUE)
        restaurant_inspections_year_critical_tdm_df <- data.frame(term =  names(restaurant_inspections_year_critical_tdm_matrix_sorted),count=restaurant_inspections_year_critical_tdm_matrix_sorted)
        
        restaurant_inspections_year_critical_tdm_df <- head(restaurant_inspections_year_critical_tdm_df, 12)
        
        ggplot(data = restaurant_inspections_year_critical_tdm_df, 
               aes(x = restaurant_inspections_year_critical_tdm_df$term, y = restaurant_inspections_year_critical_tdm_df$count)) +
            geom_bar(stat='identity') + 
            coord_flip() + theme_bw() +
            xlab("Frequency") + ylab("Term")
        
    })
    
    output$noncriticalviolations <- renderPlot({
        restaurant_inspections_year <- restaurant_inspections_filtered_3[restaurant_inspections_filtered_3$years == input$years_1,]
        restaurant_inspections_year_critical <- restaurant_inspections_filtered_3[restaurant_inspections_filtered_3$CRITICAL.FLAG == "Not Critical",]
        restaurant_inspections_year_critical <- Corpus(VectorSource(restaurant_inspections_year_critical$VIOLATION.DESCRIPTION))
        restaurant_inspections_year_critical <- tm_map(restaurant_inspections_year_critical, content_transformer(tolower))
        restaurant_inspections_year_critical <- tm_map(restaurant_inspections_year_critical, removeNumbers)
        restaurant_inspections_year_critical <- tm_map(restaurant_inspections_year_critical, removeWords, stopwords("english"))
        restaurant_inspections_year_critical <- tm_map(restaurant_inspections_year_critical, removeWords, c("used", "use", "unit", "underneath","raised", "maintained", "item", 
                                                                                                            "allow", "spaced", "preparation", "areas", "surface",
                                                                                                            "sides", "sealed", "provided", "properly", "proof", "present", "held",
                                                                                                            "exist", "attracting", "allowing", "contact",
                                                                                                            "constructed", "material", "equipment", "house", "include", "movable",
                                                                                                            "conducive", "reduced","service", "live",
                                                                                                            "may", "required", "following", "activity", "evidence", "occurred")) 
        restaurant_inspections_year_critical <- tm_map(restaurant_inspections_year_critical, removePunctuation)
        restaurant_inspections_year_critical <- tm_map(restaurant_inspections_year_critical, stripWhitespace)
        restaurant_inspections_year_critical_tdm <- TermDocumentMatrix(restaurant_inspections_year_critical)
        restaurant_inspections_year_critical_tdm_matrix <- as.matrix(restaurant_inspections_year_critical_tdm)
        restaurant_inspections_year_critical_tdm_matrix_rowsums <- rowSums(restaurant_inspections_year_critical_tdm_matrix)
        restaurant_inspections_year_critical_tdm_matrix_sorted <- sort(restaurant_inspections_year_critical_tdm_matrix_rowsums,decreasing=TRUE)
        restaurant_inspections_year_critical_tdm_df <- data.frame(term =  names(restaurant_inspections_year_critical_tdm_matrix_sorted),count=restaurant_inspections_year_critical_tdm_matrix_sorted)
        
        restaurant_inspections_year_critical_tdm_df <- head(restaurant_inspections_year_critical_tdm_df, 12)
        
        ggplot(data = restaurant_inspections_year_critical_tdm_df, 
               aes(x = restaurant_inspections_year_critical_tdm_df$term, y = restaurant_inspections_year_critical_tdm_df$count)) +
            geom_bar(stat='identity') + 
            coord_flip() + theme_bw() +
            xlab("Frequency") + ylab("Term")
        
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
