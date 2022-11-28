library(shiny)
library(shinydashboard)
library(DT)
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
ridership<- read_csv("rider_gas_complete.csv")
# Define UI for application that draws a histogram
ui <- dashboardPage( skin="purple",
  dashboardHeader(title = "Gas Price and Ridership"),
  dashboardSidebar(#add sidebar menu
    sidebarMenu(
      menuItem("Single Route", tabName = "Single_Route", icon = icon("car")),
      menuItem("Route Class", tabName = "Route_Class", icon = icon("road"))
    )),
  dashboardBody(#Add tab Item
    tabItems(
      tabItem("Single_Route",
              box(leafletOutput("routesLeaflet"), width = 8),
              # drop down list(add interactivity):set ID, add lable feature, a vector with possible choices
              box(
                selectInput("pick_routes", "choose a route:", choices = c(unique(map$route))), width = 4
              ),
              box(plotOutput("ridership_plot"), width = 6, length= 4),
              box(plotOutput("gas_price_plot"), width = 6, length= 4)),
      tabItem("cars",
              fluidPage(h1("Cars"),
                        # add data table item
                        dataTableOutput("carstable")
              ))
     
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  filtered <- reactive({
    filter(map, map$route == input$pick_routes)
  })
  filtere_rider <- reactive({
    ridership %>%
      filter(Route == input$pick_routes)
  })
  filteredClass<- reactive({
    filter(map, map$RouteClass %in%  c(input$classSelect))
  })
  output$ridership_plot <- renderPlot({
     
    ggplot(filtere_rider(),aes(x = date))+
      geom_line(aes(y = riders/coef-1),color = "#3F4345")+ 
      scale_y_continuous(
        name = NULL,
        sec.axis = sec_axis(trans = ~.*coef-1))+
      labs(title = "Rides of Selected Routes Over Time",
           subtitle = "From Jan 2014 to Oct 2017")+
      geom_text(aes(x = 2017.5, y = 2.85), label = "Riders",color = "#3F4345",size = 4)+
      theme_minimal()+
      theme(axis.text.y.left =element_blank(),
            axis.title.y.right = element_text(color = "#3F4345",size = 12),
            
            axis.text.y.right = element_text(color = "#3F4345",size = 8),
            axis.title.x.bottom = element_blank(),
            plot.title = element_text(color = "#006bb3",size = 14),
            plot.subtitle = element_text(color = "#006bb3"))
  })
  output$gas_price_plot <- renderPlot({
  
    ggplot(filtere_rider(),aes(x = date)) +
      geom_line(aes(y = gas_price), color = "#9F2C2C") +
      scale_y_continuous(name = NULL,) +
      scale_x_continuous(name = NULL,) +
      labs(title = "Gas Price Over Time",
           subtitle = "From Jan 2014 to Oct 2017") +
      geom_text(
        aes(x = 2017, y = 2.85),
        label = "Gas Price",
        color = "#9F2C2C",
        size = 4
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#006bb3", size = 14),
        plot.subtitle = element_text(color = "#006bb3")
      )
  })
  #create map
  output$routesLeaflet <- renderLeaflet({
    #setup map
    routesLeaflet <-
      #map%>%
      leaflet() %>%
      addProviderTiles("CartoDB.Positron",
                       options = providerTileOptions(opacity = 0.8)) %>%
      addPolylines(
        data = filtered(),
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
      )
    #call map
    routesLeaflet
  })
  #render the car table, just put in the data name
  output$carstable <- renderDataTable(mtcars)
}

# Run the application 
shinyApp(ui = ui, server = server)
