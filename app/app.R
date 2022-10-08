library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("NYC Crime Statistics"),
    mainPanel(leafletOutput("map"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    color <- colorFactor(topo.colors(2), total1$criticalYN)

    output$map <- renderLeaflet({
        leaflet(total1) %>%
            setView(lng = -73.98928, lat = 40.75042, zoom = 10) %>%
            addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
            addCircleMarkers(
                lng=~total1$Longitude, # Longitude coordinates
                lat=~total1$Latitude, # Latitude coordinates
                radius=~ifelse(total1$criticalYN == "Critical", 2, 2), # Total count
                stroke=FALSE, # Circle stroke
                fillOpacity=1, # Circle Fill Opacity
               color=~color(total1$criticalYN),
               popup=~paste(
                   "<b>", criticalYN , "</b><br/>",
                   "Place: ", as.character(total1$DBA), "<br/>",
                   "Borough:", as.character(total1$BORO),
                   "Action:", as.character(total1$ACTION),
                   "date: ", as.character(total1$dateandtime
                                          )
               )
                # Popup content
            ) %>%
            addLegend(
                "bottomleft", # Legend position
                pal=color, # color palette
                values=~total1$criticalYN, # legend values
                opacity = 1,
                title="Type of Crime Committed"
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
