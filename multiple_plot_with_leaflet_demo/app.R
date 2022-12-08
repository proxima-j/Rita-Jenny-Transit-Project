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
library(plotly)
library(lubridate)
#load data
map <- readOGR("newmap.shp") %>%
  st_as_sf()
ridership<- read_csv("single_route.csv")

# Define UI for application that draws a histogram
ui <- dashboardPage( skin="purple",
  dashboardHeader(title = "Gas Price and Ridership"),
  dashboardSidebar(#add sidebar menu
    sidebarMenu(
      menuItem("Single Route", tabName = "Single_Route", icon = icon("car")),
      menuItem("Route Class", tabName = "Route_Class", icon = icon("road"))
    )),
  dashboardBody(
    #Add tab Item
    tabItems(
      tabItem("Single_Route",
              fluidRow(
                box(leafletOutput("routesLeaflet"), width = 8),
                # drop down list(add interactivity):set ID, add lable feature, a vector with possible choices
                box(
                  selectInput("pick_routes", "choose a route:",
                              choices = c(unique(ridership$Route))),
                  width = 4
                ),
                #radio button
                box(
                  radioButtons(
                    "rider_data_pick",
                    "Choice of Daily Riders",
                    choices = c(
                      "Daily Riders and Gas Price",
                      "Daily Riders(removed seasonality) and Gas Price"
                    ),
                    selected = "Daily Riders and Gas Price"
                    
                  ),
                  width = 4
                )
              ),
     
             
              
              box(plotlyOutput("ridership_plot"), width = 10)),
             # box(plotOutput("gas_price_plot"), width = 6, length= 4)),
      tabItem("cars",
              fluidPage(h1("Cars"),
                        # add data table item
                        dataTableOutput("carstable")
              ))
     
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  filtered <- reactive({
    filter(map, map$route == input$pick_routes)
  })
  filtere_rider <- reactive({
    ridership %>%
      filter(Route == input$pick_routes)
  })
  filteredClass<- reactive({
    filter(ridership, ridership$RouteClass %in%  c(input$classSelect))
  })

  output$ridership_plot <- renderPlotly({
    #response to the button
    if(input$rider_data_pick=="Daily Riders and Gas Price")
    {rider_line <- filtere_rider()$`Daily Riders and Gas Price Line Plot`
     Riders=filtere_rider()$`Daily Riders`}
    if(input$rider_data_pick== "Daily Riders(removed seasonality) and Gas Price")
    {rider_line <- filtere_rider()$`norm_de_sea`
      Riders=filtere_rider()$`Daily Riders(removed seasonality)`}
    #line plot
    ridership_ggplot <- ggplot(filtere_rider(), aes(x = date)) +
      geom_line(aes(y = gas_price, label=Date), color = "#9F2C2C") +
      geom_line(aes(y = rider_line, label = Riders ), color = "#3F4345") +
      scale_y_continuous(name = "Gas Price($)",
                         sec.axis = sec_axis(trans = ~ ., name = "Total Riders(thousands)")) +
      labs(title = "Riders of Selected Routes Over Time",
           subtitle = "From Jan 2014 to Oct 2017") +
      geom_text(aes(x = max(date), y = 2.4), label = "Gas Price",color = "#9F2C2C",size = 3)+
      geom_text(aes(x = max(date), y = 1.7), label = "Total Riders",color = "#3F4345",size = 3)+
      theme_minimal() +
      theme(
        axis.title.y = element_text(color = "#9F2C2C",size = 12),
        axis.text.y= element_text(color = "#9F2C2C",size = 8),
        axis.title.x.bottom = element_blank(),
        plot.title = element_text(color = "#006bb3",size = 14),
        plot.subtitle = element_text(color = "#006bb3")
      )
    
    ggplotly(ridership_ggplot, tooltip = c("label")) %>% 
      layout(hovermode = "x unified")
    
  })
  # output$gas_price_plot <- renderPlot({
  # 
  #   ggplot(filtere_rider(),aes(x = date)) +
  #     geom_line(aes(y = gas_price), color = "#9F2C2C") +
  #     scale_y_continuous(name = NULL,) +
  #     scale_x_continuous(name = NULL,) +
  #     labs(title = "Gas Price Over Time",
  #          subtitle = "From Jan 2014 to Oct 2017") +
  #     theme_minimal() +
  #     theme(
  #       plot.title = element_text(color = "#006bb3", size = 14),
  #       plot.subtitle = element_text(color = "#006bb3")
  #     )
  # })
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
