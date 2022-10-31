#map direction of travel of selected ship

#load library
library(shiny)
library(readr)
library(leaflet)
library(tidyverse)
library(dplyr)
#load data
ais2022_01 <- read_csv("ais2022_01.csv") %>% 
  select(c("MMSI", "LON", "LAT")) %>% 
  filter(MMSI %in% c("368084090","368140160","366941830","316005971","316004054"
                     ,"366863870","368118150","338379253","368195460","367614790"))


ui <- fluidPage(

    # Application title
    titlePanel("Map of path travelled by ship"),
   

        # Show a plot of the generated distribution
        mainPanel(
              #Dropdown list of ships
             uiOutput("ship_select"),
          
              #Map
              leafletOutput("locations")
        )
    
)


server <- function(input, output) {
  
  
      #creat ship dropdown list
      output$ship_select <- renderUI({
        #we want the ship name only appears once
        selectInput("pick_ship","choose a ship:", choices= c(unique(ais2022_01$MMSI)))
      })
     
      #create map
      output$locations <- renderLeaflet({
       
        #make it only showed the ship selected
        filtered=subset(ais2022_01, ais2022_01$MMSI==input$pick_ship)
        #setup map
        locations<-leaflet(data = filtered) %>%  
          addProviderTiles("Stamen.Toner") %>%
          addProviderTiles("CartoDB.Positron",
                           options = providerTileOptions(opacity = 0.35))
         

        #also show the travel direction(increasing th opacity)
        rows<-nrow(filtered)
        x<-1/rows
        y<-0
        for(i in 0:rows){
        y<-y+x
        locations<- addCircles(locations, lng = ~LON[i], lat = ~LAT[i],color = "red",fillColor = "red", opacity=y, fillOpacity = y)
        }
        #call map
        locations
      })
     
}

# Run the application 
shinyApp(ui = ui, server = server)
