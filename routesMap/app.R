#library
library(shiny)
library(rgdal)
library(leaflet)
library (raster) 
library(dplyr)
library(sf)

#load data
map <- readOGR("C:/Users/12235/Documents/stat453/Rita-Jenny-Transit-Project/routesMap/newmap.shp") %>% 
  st_as_sf()

#vector with column names selected from map
myChoices=names(unique(map[c("RouteClass")]))
ui <- fluidPage(

    # Application title
    titlePanel("Twin Cities Metro Route Map"),


        # Show a plot of the generated distribution
        mainPanel(
          #Dropdown list of routes
          uiOutput("routes_select"),
          #Dropdown list of route class
          uiOutput("classes_select "),
          
          #Map
          leafletOutput("routesLeaflet")
        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #creat routes dropdown list
  output$routes_select <- renderUI({
    #we want the routes name only appears once
    selectInput("pick_routes","choose a route:", choices= c(unique(map$route)))
  })
  
  #creat routes dropdown list
  output$classes_select <- renderUI({
    #we want the routes name only appears once
    selectInput("pick_classes","choose a route class:", choices= c(unique(map$RouteClass)))
  })

  #create map
  output$routesLeaflet <- renderLeaflet({
    
    #make it only showed the route selected
    filtered=filter(map, map$route==input$pick_routes)
    
    #setup map
    routesLeaflet<-filtered %>%
      leaflet() %>%
      addTiles(op) %>%
      addPolylines(
        color = "#444444",
        weight = 1,
        smoothFactor = 0.5,
        opacity = 1.0,
        highlightOptions = highlightOptions(
          color = "blue",
          weight = 2,
          bringToFront = TRUE
        ),
        popup =  paste("Route: ", map$route, "<br>")
      )


    #call map
    routesLeaflet
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
