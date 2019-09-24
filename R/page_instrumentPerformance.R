library(shiny)
library(rlist)
library(tidyverse)
library(lubridate)
library(scales)

#' pageInstrumentPerformanceUI
#'
#' UI function for the page 'Instrument performance'
#'
#' @param id Identifier for the namespace of this module
#'
#' @return A HTML tag object
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

#' pageInstrumentPerformance
#'
#' Implements the server logic for the page 'Instrument performance'.
#'
#' @param input Shiny inputs
#' @param output Shiny outputs
#' @param session Shiny session
#' @param devicesUpdated A reactive expression. Is invalidated
#'                       when a new device is added.
#' @param serverEnvironment An environment. The environment of the 
#'                          server function that calls this module.
#'                          Used to execute code in the environment of the 
#'                          main server function for the app (e.g. to 
#'                          switch between pages).
#'
#' @return No explicit return value
pageInstrumentPerformance <- function(input, output, session, devicesUpdated, serverEnvironment){
  
  # ------------ RENDER SELECTION OPTIONS -----------
  
  updateSelectizeInput(session, "instruments", choices = getDevicesAsStrings())
  observeEvent(
    devicesUpdated(), 
    updateSelectizeInput(session, "instruments", choices = getDevicesAsStrings())
  )
  
  # ------------ DISPLAY HELP MESSAGE ----------
  
  observe({
    
    datasets <- getDataForDevicesAndDaterange(input$instruments, input$dateRange[1], input$dateRange[2])
    datasetsWithProcessedData <- datasets[file.exists(file.path(datasets, "processed.csv"))]
    
    output$helpMessage <- renderText(
      sprintf("Found %s datasets for the selected device(s) and timespan. 
              Out of these datasets %s have been processed.", length(datasets), length(datasetsWithProcessedData)))
  })
  
  # ------------- PLOTS ---------------
  
  observeEvent(input$go, {
    
    datasets <- getDataForDevicesAndDaterange(input$instruments, input$dateRange[1], input$dateRange[2])
    datasetsWithProcessedData <- datasets[file.exists(file.path(datasets, "processed.csv"))]
    
    req(datasetsWithProcessedData)
    
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
        scale_x_date(labels = date_format("%d-%m-%Y")) +
        labs(title = "Deviation of control standard (delta O18)")
    })
    output$plot2 <- renderPlot({
      ggplot(deviations) +
        geom_point(mapping = aes(x = date, y = devH2)) +
        scale_x_date(labels = date_format("%d-%m-%Y")) +
        labs(title = "Deviation of control standard (delta H2)")
    })
  })
}

##################
# HELPERS
##################

getDataForDevicesAndDaterange <- function(devices, startDate, endDate, basePath = BASE_PATH){
  
  dateInterval <- lubridate::interval(ymd(startDate), ymd(endDate))
  
  infoFiles <- list.files(basePath, pattern = "fileInfo.json", recursive = T, full.names = T)
  relevantInfoFiles <- list.filter(infoFiles, ~ {
    info <- list.load(.)
    info$device %in% devices && ymd(info$date) %within% dateInterval
  })
  
  datasetPaths <- str_split(relevantInfoFiles, "/") %>%
    map(~ head(., -1)) %>%
    map_chr(~ paste(., collapse = "/"))
  
  return(datasetPaths)
}
