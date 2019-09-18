library(shiny)
library(tidyverse)
library(rlist)

pageProcessDataPlotsUI <- function(id){
  
  # create namespace function
  ns <- NS(id)
  
  tagList(
    
    selectInput(ns("datasetForPlotting"), "Which dataset would you like to plot?", choices = c()),
    hr(),
    
    fluidRow(
      column(3,
             
        p(strong("data")),
        actionButton(ns("dataRaw"), "Raw"),
        actionButton(ns("dataMemoryCorrected"), "Memory corrected"),
        actionButton(ns("dataCalibrated"), "Calibrated"),
        actionButton(ns("dataDrift"), "Calibrated and drift corrected"),
        actionButton(ns("dataProcessed"), "Processed"),
        
        hr(),
        
        p(strong("injection stats")),
        actionButton(ns("injWaterLevel"), "Water level"),
        actionButton(ns("injStdDev"), "Standard deviation"),
        
        hr(),
        
        p(strong("sample stats")),
        actionButton(ns("sampleWaterLevel"), "Water level"),
        actionButton(ns("sampleStdDev"), "Standard deviation"),
        actionButton(ns("sampleDevFromTrue"), "Deviation from true value (for standards)"),
        
        hr(),
        
        p(strong("file stats")),
        actionButton(ns("fileGeneralStats"), "General stats"),
        actionButton(ns("fileMemoryCorrection"), "Memory correction"),
        actionButton(ns("fileCalibration"), "Calibration"),
        actionButton(ns("fileDriftCorrection"), "Drift Correction")
      ),
      
      column(9, offset = 3,
        uiOutput(ns("plotOutput"))
      )
    )
  )
  
}

pageProcessDataPlots <- function(input, output, session, id, 
                                 processingSuccessful, processedData){

    # ---------------- CREATE OWN REACTIVE VALUES ---------
  
  rv <- reactiveValues()
  rv$dataToPlot <- NULL
  
  # create a namespace function
  ns <- NS(id)
  
  # ----------- UPDATE LIST OF SELECTABLE DATASETS ---------
  
  observeEvent(processingSuccessful(), {
    updateSelectInput(
      session, "datasetForPlotting", 
      choices = map_chr(processedData(), ~ {assign("temp", ., envir = globalenv()); .[[1]]$name})
    )
  })
  
  # update rv$dataToPlot when a dataset is selected
  observeEvent(input$datasetForPlotting, {
    name <- input$datasetForPlotting
    rv$dataToPlot <- list.filter(processedData(), ~ name == .[[1]]$name) %>%
      first() %>%
      .[[1]]
    flog.debug("updated dataset to plot")
  })
}