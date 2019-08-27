library(tidyverse)
library(rhandsontable)
library(piccr)

pageProcessDataUI <- function(id){
  
  # create namespace function
  ns <- NS(id)
  
  tagList(
    h2("Process isotope measurement data"),
    wellPanel(
      h4("Setup and Options"), br(),
      fileInput(ns("files"), "Upload measurement files", multiple = TRUE),
      p(strong("Set the processing options for the standards")),
      rHandsontableOutput(ns("processingTemplate")), p(""), br(),
      radioButtons(ns("useMemoryCorrection"), "Use memory correction?", c("Yes" = TRUE, "No" = FALSE)),
      radioButtons(ns("useThreePointCalibration"), "Use three-point calibration or two-point calibration?", 
                   c("Use three-point calibration" = TRUE, "Use two-point calibration" = FALSE)),
      radioButtons(ns("driftAndCalibration"), "Drift correction and calibration options", 
                   c("Use linear drift correction and calibration" = 1, 
                     "Use double calibration" = 2,
                     "Use only calibration, without drift correction" = 0)),
      h4("All set up?"), br(),
      actionButton(ns("doProcess"), "Process the data", style = blue),
      downloadButton(ns("download"), "Download the processed data")
      
      # TODO: implement save on server
      # actionButton(ns("doSave"), "Save the processed data on the server")
    )
  )
}

pageProcessData <- function(input, output, session){
  
  rv <- reactiveValues()
  rv$processedData <- NULL
  
  processingTemplateInitial <- tribble(
    ~`Identifier 1`, ~`Use for drift correction`, ~`Use for calibration`, ~`Use as control standard`, ~`True delta O18`, ~`True delta H2`,
    # ------------ / -------------------------- / --------------------- / ------------------------- / ---------------- / ----------------
    "KARA",              FALSE,                         FALSE,                    FALSE,                  -0.1,                 2,
    "DML",               TRUE,                          TRUE,                     FALSE,                  -42.5,                 2,
    "TD1",               TRUE,                          TRUE,                     FALSE,                  -33.9,                 2,
    "JASE",              TRUE,                          TRUE,                     FALSE,                  -50.22,                 2,
    "NGT",               FALSE,                         FALSE,                    FALSE,                  -34.4,                 2
  )
  output$processingTemplate <- renderRHandsontable(rhandsontable(processingTemplateInitial))
  
  observeEvent(input$doProcess, {
    
    req(input$files)
    
    processingTemplateTable <- hot_to_r(input$processingTemplate)
    rv$processedData <- process(input, processingTemplateTable)
  })
  
  output$download <- downloadHandler(
    filename = "processed.zip",
    content = function(file) {
      processedData <- rv$processedData$processed
      downloadProcessedData(file, processedData)
    }
  )
}

process <- function(input, processingTemplate){
  
  standards <- processingTemplate %>%
    transmute(name = `Identifier 1`, o18_True = `True delta O18`, H2_True = `True delta H2`, 
              use_for_drift_correction = `Use for drift correction`, use_for_calibration = `Use for calibration`) %>% 
    transpose() 
  
  config <- list(
    standards = standards,
    average_over_last_n_inj = 3,
    use_three_point_calibration = input$useThreePointCalibration,
    use_memory_correction = input$useMemoryCorrection,
    calibration_method = input$driftAndCalibration
  )
  
  tibble.print_min <- 30
  
  datasets <- map(input$files$datapath, ~ read_csv(.))
  names(datasets) <- input$files$name
  
  piccr::processData(datasets, config)
}

downloadProcessedData <- function(file, processedData){
  
  #go to a temp dir to avoid permission issues
  owd <- setwd(tempdir())
  on.exit(setwd(owd))
  
  filenames <- names(processedData)
  walk(filenames, ~ write_csv(processedData[[.]], .))
  
  # zip does not override existing files
  file.remove(file)
  
  zip(file, filenames)
}