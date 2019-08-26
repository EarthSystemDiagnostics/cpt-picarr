library(shiny)
library(rhandsontable)
library(ggplot2)
library(readr)

source("page_processData.R")

ui <- navbarPage(
    "Cpt. Picarr", id = "page",
    
    tabPanel(
      "Process measurement data",
      pageProcessDataUI("processData")
    )
)

server <- function(input, output, session){
  
  callModule(pageProcessData, "processData")
}

shinyApp(ui, server)