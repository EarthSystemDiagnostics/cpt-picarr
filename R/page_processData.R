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
      radioButtons(ns("driftAndCalibration"), "Drift correction and calibration options", 
                   choiceNames = list(
                     "Use linear drift correction and two-point calibration" ,
                     "Use linear drift correction and three-point calibration",
                     "Use double two-point calibration",
                     "Use double three-point calibration",
                     "Use only two-point calibration, without drift correction",
                     "Use only three-point calibration, without drift correction"),
                   choiceValues = list(
                     # "x/y": x is the calibration flag, y indicates if three-point calibration is to be used.
                     "1/F", "1/T", "2/F", "2/T", "0/F", "0/T"
                   )),
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


#######################
# HELPERS
#######################

process <- function(input, processingTemplate, datasets){
  
  standards <- processingTemplate %>%
    transmute(name = `Identifier 1`, o18_True = `True delta O18`, H2_True = `True delta H2`, 
              use_for_drift_correction = `Use for drift correction`, use_for_calibration = `Use for calibration`) %>% 
    transpose() 
  
  calibrationMethod <- as.numeric(str_split(input$driftAndCalibration, "/")[[1]][[1]])
  useThreePointCalibration <- as.logical(str_split(input$driftAndCalibration, "/")[[1]][[2]])
  
  config <- list(
    standards = standards,
    average_over_last_n_inj = 3,
    use_memory_correction = input$useMemoryCorrection,
    calibration_method = calibrationMethod,
    use_three_point_calibration = useThreePointCalibration
  )
  
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