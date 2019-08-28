library(tidyverse)
library(rhandsontable)
library(piccr)
library(futile.logger)

pageProcessDataUI <- function(id){
  
  # create namespace function
  ns <- NS(id)
  
  tagList(
    h2("Process isotope measurement data"),
    wellPanel(
      h4("Setup and Options"), br(),
      fileInput(ns("files"), "Upload measurement files", multiple = TRUE),
      p(strong("Set the processing options for the standards")),
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
      downloadButton(ns("download"), "Download the processed data"),
      textOutput(ns("helpMessage"))
      
      # TODO: implement save on server
      # actionButton(ns("doSave"), "Save the processed data on the server")
    )
  )
}

pageProcessData <- function(input, output, session){
  
  rv <- reactiveValues()
  rv$processedData <- NULL
  
  observeEvent(input$doProcess, {
    req(input$files)
    tryCatch({
        rv$processedData <- process(input, BASE_PATH)
        output$helpMessage <- renderText("Data processed successfully.")
      }, error = function(errorMessage) {
        output$helpMessage <- renderText("An error occured and the data could not be processed.")
        flog.error(errorMessage)
      }
    )
  })
  
  output$download <- downloadHandler(
    filename = "processed.zip",
    content = function(file) {
      print("called")
      processedData <- rv$processedData
      downloadProcessedData(file, processedData)
    }
  )
}


#######################
# HELPERS
#######################

process <- function(input, basePath){
  
  flog.info(str_c("Processing datasets"))
  flog.debug(str_c("basePath: ", basePath))
  
  flog.debug("Read input datasets")
  datasets <- readInputDatasets(input)
  flog.debug(str_c("loaded datasets: ", do.call(paste, as.list(names(datasets)))))
  
  flog.debug("Loading processing options")
  processingOptions <- loadProcessingOptions(datasets, basePath)
  
  flog.debug("Generating config")
  config <- generateConfig(input)
  
  flog.debug("Processing the loaded datasets")
  processedData <- processDatasets(datasets, processingOptions, config)
  
  flog.debug("Outputting the processed data")
  return(processedData)
}

readInputDatasets <- function(input){
  datasets <- map(input$files$datapath, ~ read_csv(.))
  names(datasets) <- input$files$name
  return(datasets)
}

loadProcessingOptions <- function(datasets, basePath){
  # Load processing Options for each dataset from disc. (Using the unique identifier coded
  # into the `Identifier 2` column)
  map(datasets, function(dataset){
    firstIdentifier2 <- first(dataset$`Identifier 2`)
    uniqueIdentifier <- str_extract(firstIdentifier2, "(?<=_).+$")
    
    path <- file.path(basePath, "data", uniqueIdentifier)
    processingOptions <- read_csv(file.path(path, "processingOptions.csv"))
    return(processingOptions)
  })
}

generateConfig <- function(input){
  calibrationMethod <- as.numeric(str_split(input$driftAndCalibration, "/")[[1]][[1]])
  useThreePointCalibration <- as.logical(str_split(input$driftAndCalibration, "/")[[1]][[2]])
  
  config <- list(
    average_over_last_n_inj = 3,
    use_memory_correction = input$useMemoryCorrection,
    calibration_method = calibrationMethod,
    use_three_point_calibration = useThreePointCalibration
  )
  return(config)
}

processDatasets <- function(datasets, processingOptions, config){
  
  map2(datasets, processingOptions, function(dataset, processingOptions, config){
  
      standards <- processingOptions %>%
        transmute(name = `Identifier 1`, o18_True = `True delta O18`, H2_True = `True delta H2`, 
                  use_for_drift_correction = `Use for drift correction`, use_for_calibration = `Use for calibration`) %>% 
        transpose() 
      config$standards <- standards
    
    piccr::processData(list(data = dataset), config)
  }, config = config)
}

downloadProcessedData <- function(file, processedData){
  
  flog.info("Downloading data")
  
  #go to a temp dir to avoid permission issues
  owd <- setwd(tempdir())
  on.exit(setwd(owd))
  
  flog.debug("Writing files")
  filenames <- names(processedData)
  walk(filenames, ~ write_csv(processedData[[.]]$processed$data, .))
  flog.debug(str_c("Filenames: ", do.call(paste, as.list(filenames))))
  
  flog.debug("Creating zip archive")
  # zip does not override existing files
  file.remove(file)
  zip(file, filenames)
}