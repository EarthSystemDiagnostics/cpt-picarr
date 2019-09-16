library(shiny)
library(rlist)
library(tidyverse)
library(lubridate)

pageInstrumentPerformanceUI <- function(id){
  
  # create namespace function
  ns <- NS(id)
  
  tagList(
    h2("Instrument performance"),
    p("Take a look at cross-project statistics to analyze instrument performance."),
    p("Please note that only processed data is taken into account."),
    
    wellPanel(
      selectizeInput(ns("instruments"), "Select one or multiple measurement instruments", 
                     choices = c(), multiple = TRUE),
      dateRangeInput(ns("dateRange"), "Select a timespan to look at"),
      textOutput(ns("helpMessage")),
      br(),
      actionButton(ns("go"), "Show me stats for the selected device(s) and timespan", style = blue)
    ),
    
    plotOutput(ns("plot1")),
    plotOutput(ns("plot2"))
  )
}

pageInstrumentPerformance <- function(input, output, session, serverEnvironment){
  
  updateSelectizeInput(session, "instruments", choices = getDeviceNames())
  
  observe({
    
    force(input$instruments)
    force(input$dataRange)
    
    datasets <- getDataForDevices(input$instruments)
    datasetsWithProcessedData <- datasets[file.exists(file.path(datasets, "processed.csv"))]
    
    output$helpMessage <- renderText(
      sprintf("Found %s datasets for the selected device(s) and timespan. 
              Out of these datasets %s have been processed.", length(datasets), length(datasetsWithProcessedData)))
  })
  
  observeEvent(input$go, {
    
    datasets <- getDataForDevices(input$instruments)
    datasetsWithProcessedData <- datasets[file.exists(file.path(datasets, "processed.csv"))]
    
    deviations <- map_dfr(datasetsWithProcessedData, function(dataset) {
      processedData <- read_csv(file.path(dataset, "processed.csv"))
      processingOptions <- read_csv(file.path(dataset, "processingOptions.csv"))
      date <- list.load(file.path(dataset, "fileInfo.json")) %>%
        .$date %>%
        lubridate::ymd()
      
      controlStandard <- filter(processingOptions, `Use as control standard`)
      df <- inner_join(processedData, controlStandard)
      transmute(df, devO18 = delta.O18 - `True delta O18`, 
                devH2 = delta.H2 - `True delta H2`, date = date)
    })
    
    output$plot1 <- renderPlot({
      ggplot(deviations) +
        geom_point(mapping = aes(x = date, y = devO18)) +
        labs(title = "Deviation of control standard (delta O18)")
    })
    output$plot2 <- renderPlot({
      ggplot(deviations) +
        geom_point(mapping = aes(x = date, y = devH2)) +
        labs(title = "Deviation of control standard (delta H2)")
    })
  })
}

##################
# HELPERS
##################

getDeviceNames <- function(basePath = BASE_PATH){
  
  infoFiles <- list.files(basePath, pattern = "fileInfo.json", recursive = T, full.names = T)
  devices   <- map_chr(infoFiles, ~ list.load(.)$device)
  return(unique(devices))
}

getDataForDevices <- function(devices, basePath = BASE_PATH){
  
  infoFiles <- list.files(basePath, pattern = "fileInfo.json", recursive = T, full.names = T)
  relevantInfoFiles <- list.filter(infoFiles, ~ list.load(.)$device %in% devices)
  datasetPaths <- str_split(relevantInfoFiles, "/") %>%
    map(~ head(., -1)) %>%
    map_chr(~ paste(., collapse = "/"))
  return(datasetPaths)
}