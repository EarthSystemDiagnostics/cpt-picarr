library(shiny)

pageInstrumentPerformanceUI <- function(id){
  
  # create namespace function
  ns <- NS(id)
  
  tagList(
    h2("Instrument performance"),
    p("Take a look at cross-project statistics to analyze instrument performance."),
    
    wellPanel(
      selectizeInput(ns("instrument"), "Select one or multiple measurement instruments", 
                     choices = c("all"), multiple = TRUE),
      dateRangeInput(ns("dateRange"), "Select a timespan to look at"),
      actionButton(ns("go"), "Show me stats for the selected device(s) and timespan", style = blue)
    ),
    
    plotOutput(ns("plot"))
  )
}

pageInstrumentPerformance <- function(input, output, session, serverEnvironment){
  
}