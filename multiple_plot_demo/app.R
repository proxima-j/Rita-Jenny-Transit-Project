library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(readr)
library(tidyr)

#Data Preparation
data<-read.csv("multiple_plot_demo/data_full_with_tempreture.csv") %>% 
  mutate(DateTime= TIME) %>% 
  separate(TIME,c("Date","Time"), sep = " ")


# Define UI for application that draws a histogram
ui <- fluidPage(
  #App title
  titlePanel("shiny demo"),
  
  # sidebar layout with input and output definitions
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      # Input: slider for number
    )
  )

        )

# Define server logic required to draw a histogram
server <- function(input, output) {

 
}

# Run the application 
shinyApp(ui = ui, server = server)
