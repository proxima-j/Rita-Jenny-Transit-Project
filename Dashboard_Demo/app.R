library(shiny)
#library(shinydashboard)
#replacement of the shinydashboard library
library(semantic.dashboard)
library(DT)
# Define UI for application that draws a histogram
ui <- dashboardPage(
  #this is for semantic dashboard
  theme="slate",
  #this only for shiny dashboard
  #skin = "purple",
  dashboardHeader(title = "Dashboard Demo"),
  dashboardSidebar(#add sidebar menu
    sidebarMenu(
      menuItem("Iris", tabName = "iris", icon = icon("tree")),
      menuItem("Cars", tabName = "cars", icon = icon("car"))
    )),
  dashboardBody(#Add tab Item
    tabItems(
      tabItem("iris",
              box(plotOutput("correlation_plot"), width = 8),
              # drop down list(add interactivity):set ID, add lable feature, a vector with possible choices
              box(
                selectInput(
                  "features",
                  "Features:",
                  c("Sepal.Width", "Petal.Width", "Petal.Length")
                ), width = 4
              )),
      tabItem("cars",
              fluidPage(h1("Cars"),
                        # add data table item
                        dataTableOutput("carstable")
                        ))
      #move this whole plot to the tabItem
      # #add a box to store the plot output, ans this specific plot is named "correlation plot"
      # box(plotOutput("correlation_plot"), width= 8),
      # # drop down list(add interactivity):set ID, add lable feature, a vector with possible choices
      # box(
      #   selectInput("features", "Features:",
      #               c("Sepal.Width","Petal.Width","Petal.Length")), width = 4
      # )
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #render the correlation plot with the iris dataset
  output$correlation_plot <- renderPlot({
    #make the dropdown list the input into the plot function
    plot(iris$Sepal.Length,iris[[input$features]],
         xlab = "Sepal length", ylab = "Feature")
  })
  #render the car table, just put in the data name
  output$carstable <- renderDataTable(mtcars)
}

# Run the application 
shinyApp(ui = ui, server = server)
