#library
library(shiny)
library(shinyWidgets)
library(rgdal)
library(leaflet)
library (raster) 
library(dplyr)
library(sf)
library(tidyverse)
#load data
map <- readOGR("newmap.shp") %>% 
  st_as_sf()

#Class name for checkbox
className<- map %>% 
  distinct(RouteClass) %>% 
  mutate(dum="1") %>% 
  pivot_wider(names_from = RouteClass, values_from = dum)

mychoice=names(className)


ui <- fluidPage(

    # Application title
    titlePanel("Twin Cities Metro Route Map"),

    sidebarLayout(
      sidebarPanel(
        h4("Please choose route classes to include :"),
        #Select ALL
       checkboxInput("all","Select All/None"),
        #add checkbox group
        checkboxGroupInput("classSelect","Route Classes",mychoice)
       
        # #checkbox list of route class
        # uiOutput("classes_select "),
      ),
      # Show a plot of the generated distribution
      mainPanel(
        #Dropdown list of routes
        uiOutput("routes_select"),
        
        
        #Map
       leafletOutput("routesLeaflet"),
        
      )
    )
        
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #update checkbox input
  observe({
    updateCheckboxGroupInput(
      session,"classSelect", choices = mychoice,
      #if select all is true, then show all the choices, vice versa
      selected= if(input$all) mychoice
    )
  })
  
  #creat routes dropdown list
  output$routes_select <- renderUI({
    #we want the routes name only appears once
    selectInput("pick_routes","choose a route:", choices= c(unique(map$route)))
  })

  
  #create map
  output$routesLeaflet <- renderLeaflet({
    
    #make it only showed the route selected
    filtered=filter(map, map$route==input$pick_routes)
    #class filter
    filteredClass=filter(map, map$RouteClass %in%  c(input$classSelect))
    #setup map
    routesLeaflet<-
      #map%>%
      leaflet() %>%
      addTiles() %>%
      addPolylines(data = filtered,
        color = "navy",
        weight = 3,
        smoothFactor = 0.5,
        opacity = 1.0,
        highlightOptions = highlightOptions(
          color = "cyan",
          weight = 2,
          bringToFront = TRUE
        ),
        popup =  paste("Route: ", map$route, "<br>")
      ) %>% 
      addPolylines(data = filteredClass,
                   color = "violet",
                   weight = 1,
                   smoothFactor = 0.5,
                   opacity = 0.5,
                   highlightOptions = highlightOptions(
                     color = "yellow",
                     weight = 2,
                     bringToFront = TRUE
                   ),
                   popup =  paste("Route: ", map$route, "<br>"))

   
    #call map
    routesLeaflet
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
