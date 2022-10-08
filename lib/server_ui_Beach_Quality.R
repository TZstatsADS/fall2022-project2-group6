##### server.R
library(shiny)
library(tidyverse)
library(zoo)

beach.2020 <- read_csv("beach_2021.csv")


shinyServer(function(input, output) {
  
  
  output$plot=renderPlot({
    if (input$location == "NONE"){
      if (input$beach == "NONE"){
        data <- beach.2020 %>% 
          mutate(ym = substr(`Sample Date`, 1, 7)) %>%
          group_by(ym) %>%
          summarize(Enterococci = mean(`Enterococci Results`)) %>%
          ungroup()
        
      }else{
        data <- beach.2020 %>% 
          filter(`Beach Name` == input$beach) %>% 
          mutate(ym = substr(`Sample Date`, 1, 7)) %>%
          group_by(ym) %>%
          summarize(Enterococci = mean(`Enterococci Results`)) %>%
          ungroup()
        
      }
      
      ggplot(data=data, aes(x=as.Date(as.yearmon(ym)), y=Enterococci, group=1)) + 
        geom_line(color="grey") + 
        geom_point(color="#69b3a2", size=2) + 
        scale_x_date(date_breaks  ="6 month") +
        labs(x="Year") + 
        theme_bw() + 
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size=5))
      
    }else{
      
      if (input$beach == "NONE"){
        data = beach.2020 %>% 
          filter(`Sample Location` == input$location) %>% 
          mutate(ym = substr(`Sample Date`, 1, 7)) %>%
          group_by(ym) %>%
          summarize(Enterococci = mean(`Enterococci Results`)) %>%
          ungroup()
        
      }else{
        data <- beach.2020 %>% 
          filter(`Beach Name` == input$beach & `Sample Location` == input$location) %>% 
          mutate(ym = substr(`Sample Date`, 1, 7)) %>%
          group_by(ym) %>%
          summarize(Enterococci = mean(`Enterococci Results`)) %>%
          ungroup()
        
      }
      
      ggplot(data = data, aes(x = as.Date(as.yearmon(ym)), y = Enterococci, group=1)) +
        geom_line(color="grey") + 
        geom_point(color="#69b3a2", size=2) + 
        scale_x_date(date_breaks  ="6 month") + 
        theme_bw()+ 
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size=5)) + 
        labs(x="Year")
      
      
    }
    
    
    
  })
})


##### ui.R
library(shiny)
library(tidyverse)

beach.2020 <- read_csv("beach_2021.csv")
beaches <- append(c("NONE"), unique(beach.2020$`Beach Name`))

shinyUI(
  fluidPage(
    sidebarPanel(
      selectInput("beach", label = "Beach Name:",
                  choices = beaches, selected = "NONE"), 
      selectInput("location", label = "Sample Location:",
                  choices = c("NONE", "Center", "Left", "Right"), selected = "NONE")
    ),
    mainPanel(
      plotOutput("plot")
    )
  ))
