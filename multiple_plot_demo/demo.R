library("shiny")
library("dplyr")
library("ggplot2")
library("DT")
options(scipen = 999)
load("mamarkdowndata.rdata") # loads variable markdowndata
ma_appdata_for_map <- readRDS("zip_mass_appdata_for_map.rds")
# Define UI 
ui <- fluidPage(
  # Application title
  titlePanel("Income and Housing Costs by ZIP Code"),
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      selectInput("mycities", "Choose 1 or more Massachusetts places: ", choices = c("All Mass", sort(unique(markdowndata$City))), multiple = TRUE, selected = "Boston"),
      br(),
      strong("Note: some cities may have more than one place name for ZIP codes. For example, Allston, Brighton, Dorchester, and several other neighborhoods are not included in ZIP code place name \"Boston\".")
    ),
    # Show histogram
    mainPanel(
      h4(htmlOutput("histogramHeadline")),
      plotOutput("myhistogram"),
      br(),
      h4(htmlOutput("tableHeadline")),
      DTOutput("mytable")
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  # mydata <- reactive({
  #   if("All Mass" %in% input$mycities){
  #     markdowndata
  #   } else {
  #     filter(markdowndata, City %in% input$mycities)
  #   }
  # })
  mydata <- reactive({
    filter(markdowndata, City %in% input$mycities)
  })
  selected_places <- reactive({
    if("All Mass" %in% input$mycities){
      "Massachusetts"
    } else {
      paste(input$mycities, 
            sep = " ", collapse = ", ")
    }
  })
  output$histogramHeadline <- renderUI({
    paste("Histogram for", selected_places(), " income data")
  })
  output$tableHeadline <- renderUI({
    paste("Data for", selected_places())
  })
  output$myhistogram <- renderPlot({
    ggplot(mydata(), aes(x = MedianHouseholdIncome)) +
      geom_histogram(binwidth = 20000, color = "black", fill = "darkgreen") +
      theme_classic() +
      xlab("") +
      ylab("")  +
      scale_x_continuous(labels = dollar)
  })
  output$mytable <- renderDT({
    DT::datatable(mydata(), filter = 'top') %>%
      formatCurrency(4:5, digits = 0) %>%
      formatCurrency(6, currency = "", digits = 0)
  })
}
# Run the application 
shinyApp(ui = ui, server = server)