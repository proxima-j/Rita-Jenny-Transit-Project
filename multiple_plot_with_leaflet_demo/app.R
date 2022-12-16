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
mutilple_route<- read_csv("mutilple_route.csv")
prediction_coef<- read_csv("prediction.csv")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(
    title ="Gas Price and Ridership",
    titleWidth = 300
  ),
  dashboardSidebar(#add sidebar menu
    width = 300,
    sidebarMenu(
      menuItem("Single Route", tabName = "Single_Route", icon = icon("car")),
      menuItem("Route Class", tabName = "Route_Class", icon = icon("road")),
      menuItem("Description", tabName = "Description", icon = icon("bookmark"))
    )),
  dashboardBody(
    tags$script(HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")),
    #Add tab Item
    tabItems(
      tabItem(
        "Single_Route",
        fluidRow(
          box(leafletOutput("routesLeaflet"), width = 8),
          # drop down list(add interactivity):set ID, add lable feature, a vector with possible choices
          box(
            selectInput("pick_routes", "choose a route:",
                        choices = c(unique(ridership$Route))),
            textOutput("descrp"),
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
        
        
        box(plotlyOutput("ridership_plot"), width = 10)
        
      ),
      # box(plotOutput("gas_price_plot"), width = 6, length= 4)),
      tabItem("Route_Class",
              fluidRow(
                box(leafletOutput("classLeaflet"), width = 8),
                # add data table item
                box(
                  radioButtons(
                    "class_data_pick",
                    "Choice of Route Class",
                    choices = c("CoreLoc",
                                "CommExpress",
                                "SuburbLoc",
                                "LRT",
                                "SupportLoc"),
                    selected = "CoreLoc"
                  ),
                  width = 4
                ),
                box(plotlyOutput("prediction_plot"), width = 10),
                box(dataTableOutput("prediction_coefficient"), width = 10)
              )),
      tabItem("Description",
              h2("Motivation & Research Question", style = "font-family: 'times'; font-si20pt"),
              
              p("In recent years, the continual increase in gas prices has garnered significant attention in society. 
              These influential factors are closely tied to our daily lives, particularly in terms of transportation. 
              As such, we are interested in how policy makers and transportation planners, 
              including companies like Metro Transit, will address this fluid situation. At the same time, 
              we are also curious about how they will approach these issues in different contexts. 
                To that end, we propose the following two research questions in this research project:", 
                style = "font-family: 'times'; font-si22pt"),
              tags$li("What is the relationship between gas prices and bus ridership?", style = "color:blue"), 
              tags$li("How does this relationship differ among different types of bus routes?", style = "color:blue"), 
              h2("Dataset Introduction", style = "font-family: 'times'; font-si20pt"),
              p("In this project, we originally used four datasets: one for the leaflet from", 
               
              tags$a("'St Paul Open Information' website,", href="https://information.stpaul.gov/datasets/stpaul::district-councils/explore?location=44.962263%2C-93.033812%2C11.92"),
              "one for weekly gas prices from", 
              tags$a(href="https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=PET&s=EMM_EPMRU_PTE_SMN_DPG&f=W", 
                     "U.S. Energy Information Administration"),
              ", one for ridership data covering almost all routes between 2014 and 2017 from Metro Transit, 
              and finally, 
                one for long-term ridership data called 'Highroute' with 10 routes from",
              tags$a(href="https://www.kaggle.com/datasets/andrewosman/metro-transit-ridership-and-economic-data?select=high_route_ridership.csv", 
                                                      "Kaggle"),". 
                We renamed the leaflet dataset to 'newmap' and used it to create visualizations in a leaflet. 
                The gas price dataset was aggregated at the monthly level, and we calculated the average price for 
                each month, including a three-month moving average. Then we combined this gas price data with the 
                ridership data from 2014 to 2017.", style = "font-family: 'times'; font-si22pt"),
              p("After that, we transform the daily data originally in this 2014 to 2017 dataset into 
              monthly data and group it by route class to generate the total ridership for each class. 
              Continuing to generate the ridership for each single route, we combine the two original 
              ridership datasets together and separate routes into two categories: those highly 
              influenced by weekends, such as commuter routes, and others. For the former, 
              we calculate the daily ridership by dividing the monthly total by the number 
              of weekdays in the corresponding month. 
                For the latter, we simply divide the monthly total by the number of days 
                in the corresponding month.", style = "font-family: 'times'; font-si22pt"),
              p("Thus, we are left with three datasets: ‘newmap’ for leaflet visualizations, 
              ‘single_route’ containing ridership data for each route, and ‘multiple_route’ 
              containing ridership data for each route class. 
                Both ridership datasets include gas price data from the gas price dataset.",
                style = "font-family: 'times'; font-si22pt"),
              h2("Model Introduction", style = "font-family: 'times'; font-si20pt"),
              p("For a single route, we assume that the relationship between gas price and ridership 
              would be affected by seasonality, based on our pre-visualization demonstrations where 
              we graphed a single bus route with gas price and observed some seasonality patterns. 
              Therefore, we decided to use a time series model on this dataset to see if there is still 
              a relationship between ridership and gas price after removing seasonality. In order to do this, 
              we used a linear regression model that factors by month to get the residual, which is ridership 
              without seasonality. 
                Then we plotted the residuals with gas price to see the relationship between them.", 
                style = "font-family: 'times'; font-si22pt"),
              p("For route classes, we continued to use the linear regression model and added 
              gas price as a variable in the model. This allowed us to see the coefficient of the model, 
                such as an estimate, and the p-value of the model.", style = "font-family: 'times'; font-si22pt"),
              h2("Shiny App Demo", style = "font-family: 'times'; font-si20pt"),
              p("To better communicate and expand the accessibility of the audience, 
              we decided to implement our result into an interactive web application called Shiny App. 
              In this shiny app, we have included three pages,
                two for interactive visualization and one for narrative.", style = "font-family: 'times'; font-si22pt"),
              p("For the visualization pages, we separated the bus routes into two different categories: 
              single route and route class. Both pages include a Leaflet Map with popup information about 
              the routes to help the user explicitly navigate bus routes. Other visualization graphs including 
              line graphs comparing the morph of 
                gas price and ridership are also being displayed to the user.", 
                style = "font-family: 'times'; font-si22pt"),
              h2("Conclusion", style = "font-family: 'times'; font-si20pt"),
              p("For the", tags$a("single route", onclick = "openTab('Single_Route')", href="#"),", we can observe from the line graph that some routes have a similar 
              pattern as the gas price, but not all routes do. In general, the pattern of ridership for single 
                routes and gas price is similar, but it varies a lot by route.", 
                style = "font-family: 'times'; font-si22pt"),
              p("For" , tags$a("multiple routes", onclick = "openTab('Route_Class')", href="#"),", or route classes, we observed that in some classes, 
              such as CoreLoc and CommExpress, the estimated coefficient is positive and the p-value is small, 
              indicating that our initial hypothesis that there is some relationship between these variables 
              is reasonable. On the other hand, for some other route classes, the estimated coefficient is 
                negative and the p-value is relatively large. In this case, the result is not feasible.", 
                style = "font-family: 'times'; font-si22pt"),
              p("In conclusion, we can state that the ridership of some of the routes or 
              route classes have a similar pattern that corresponds to the gas price. However, 
              we cannot conclude that gas price would influence or cause changes in ridership 
              without considering all the possible confounding variables, and it is important to 
              remember that association does not imply causality. Additionally, the variation among 
              different route classes answered the second research question, 
                showing how the relationship between ridership and gas price differs among different route types.", 
                style = "font-family: 'times'; font-si22pt"),
              h2("Limitation", style = "font-family: 'times'; font-si20pt"),
              p("Due to the limitations of the dataset we have, 
              we are only able to analyze data from 2014 to 2017, 
              which is a relatively short time period for studying the effects of gas price on ridership. 
              Although we added another dataset that contains data from a longer time period, 
              it contains fewer bus routes than the original dataset we used. In addition, 
              the most recent data in the dataset is from 2017, which is quite a long time ago, 
              especially given the changes that have occurred since then, such as the ongoing pandemic. 
              Missing or unusual data is another problem that we were unable to address in our research. 
              For example, we decided to filter out the CommonRail type of routes in our dataset because 
              it only contains one and a half years of data for one single route in this category. 
              Therefore, it would not be reasonable to factor by month or make predictions using this data. 
              For future studies, it would be necessary to collect more data, and it would also be worthwhile 
                to study other factors that may affect ridership, such as demographic information of riders.", 
                style = "font-family: 'times'; font-si22pt"),
              h2("Group Members", style = "font-family: 'times'; font-si20pt"),
              p("Hello, we are Rita and Jenny, two Macalester College students studying data science. 
              In this project, Rita is mainly focusing on cleaning the dataset and building the 
              models while Jenny focuses more on the Shiny app and the paper works. Together, 
              the whole project is being equally distributed between our two members. 
                If you are interested in further exploring this project here is the link to the",
                tags$a(href="https://github.com/proxima-j/Rita-Jenny-Transit-Project.git", "project repository"),
                ", which includes our code and datasets. The readme file has clear instructions for users, 
                you could follow those as needed.", style = "font-family: 'times'; font-si22pt")
              )
      
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  filtered <- reactive({
    filter(map, map$route == input$pick_routes)
  })
  filtered_route_map <- reactive({
    filter(map, map$RouteClass == input$class_data_pick)
  })
  filtere_rider <- reactive({
    ridership %>%
      filter(Route == input$pick_routes)
  })
  
  filteredClass<- reactive({
    filter(mutilple_route, mutilple_route$RouteClass == input$class_data_pick)
  })
  
  output$descrp <- renderText({
    paste("Route Description:    ",filtere_rider()$dscp %>% unique(),sep = "\n")
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
      geom_text(aes(x = max(date), y = 2.4), label = "Gas Price",color = "#9F2C2C",size = 2.5)+
      geom_text(aes(x = max(date), y = 1.7), label = "Total Riders",color = "#3F4345",size = 2.5)+
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

  #data table
  output$prediction_coefficient <- renderDataTable({
    datatable(prediction_coef,extensions = "Buttons", options = list(dom="Blfrtip", button=c("copy","pdf")))
  })
  output$prediction_plot <- renderPlotly({
    #line plot
    prediction_plot <- ggplot(filteredClass(), aes(x = date)) +
      geom_line(aes(y = monthly_riders), color = "#9F2C2C") +
      geom_line(aes(y = prediction), color = "#3F4345") +
      scale_y_continuous(name = "Monthly Riders",
                         sec.axis = sec_axis(trans = ~ ., name = "Gas Price($)")) +
      labs(title = "Riders of Selected Route Classes Over Time",
           subtitle = "From Jan 2014 to Oct 2017") +
      theme_minimal() +
      theme(
        axis.title.y = element_text(color = "#9F2C2C", size = 12),
        axis.text.y = element_text(color = "#9F2C2C", size = 8),
        axis.title.x.bottom = element_blank(),
        plot.title = element_text(color = "#006bb3", size = 14),
        plot.subtitle = element_text(color = "#006bb3")
      )
    
    ggplotly(prediction_plot) %>%
      layout(hovermode = "x unified")
    
  })
  
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
  
  output$classLeaflet <- renderLeaflet({
    #setup map
    classLeaflet <-
      #map%>%
      leaflet() %>%
      addProviderTiles("CartoDB.Positron",
                       options = providerTileOptions(opacity = 0.8)) %>%
      addPolylines(
        data =  filtered_route_map(),
        weight = 3,
        smoothFactor = 0.5,
        opacity = 0.5,
        highlightOptions = highlightOptions(
          color = "yellow",
          weight = 2,
          bringToFront = TRUE
        ),
        popup =  paste("Route: ", map$route, "<br>")
      )
    
    #call map
    classLeaflet
  })
  #render the car table, just put in the data name
  output$carstable <- renderDataTable(mtcars)
}

# Run the application 
shinyApp(ui = ui, server = server)
