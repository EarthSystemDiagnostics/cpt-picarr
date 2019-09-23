library(shiny)
library(tidyverse)
library(futile.logger)

pageProcessDataOptionsUI <- function(id){
  
  # create namespace function
  ns <- NS(id)
  
  wellPanel(
    h4("Setup and Options"), br(),
    radioButtons(ns("selectionType"), "Select datasets by name or by date?", 
                 choices = list("Select datasets by name" = "name", "Select datasets by date" = "date"),
                 selected = NA),
    uiOutput(ns("selectionSpace")),
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
    selectizeInput(
      ns("averageOverInj"), "Average over the last n injections (selecting datasets updates choices)", c("all")),
    h4("All set up?"), br(),
    actionButton(ns("doProcess"), "Process the data", style = blue),
    downloadButton(ns("download"), "Download the processed data"),
    textOutput(ns("helpMessage"))
  )
  
}

pageProcessDataOptions <- function(input, output, session, id, projectDataChanged, project, serverEnvironment){
  
  # ---------------- CREATE OWN REACTIVE VALUES ---------
  
  rv <- reactiveValues()
  rv$datasetNames <- c()  # type: vector of character vectors
  
  # create a namespace function
  ns <- NS(id)
  
  # ---------------- MANAGE STATE -------------------
  
  # update the list of selectable datasets when a 
  # project is loaded or a dataset is uploaded.
  observe({
    # update list of selectable datasets when a selection 
    # type is chosen or a project is loaded
    force(projectDataChanged()) 
    force(input$selectionType)
    
    
    datasets <- getNamesOfDatasetsInProject(project())
    updateSelectizeInput(session, "datasetNames", choices = datasets)
    flog.debug("updated dataset selection on page 'process data' (project : %s)", project())
  })
  
  # keep rv$datasetNames up to date
  observeEvent(input$datasetNames, {
    rv$datasetNames <- input$datasetNames
    flog.debug("Selected datasets: %s", paste(rv$datasetNames, collapse = ", "))
  })
  observeEvent(input$dateRange, {
    rv$datasetNames <- getDatasetsInDateRange(input$dateRange, project())
    flog.debug("Selected datasets: %s", paste(rv$datasetNames, collapse = ", "))
    output$datesSelectedMessage <- renderText(
      sprintf("%s datasets in this timespan: %s", length(rv$datasetNames), 
              paste(rv$datasetNames, collapse = ", ")))
  })
  
  # update possible choices for number of injections to average over
  observeEvent(rv$datasetNames, {
    req(rv$datasetNames)
    minInjCount <- getMinInjectionCount(rv$datasetNames, project())
    updateSelectInput(session, "averageOverInj", choices = c("all", 1:minInjCount))
  })
  
  # --------------- RENDER DATASET SELECTION SECTION -------------
  
  observeEvent(input$selectionType, {
    if (input$selectionType == "date")
      element <- tagList(
        dateRangeInput(ns("dateRange"), "Process all the data in this timespan"),
        textOutput(ns("datesSelectedMessage")),
        br()
      )
    else
      element <- selectizeInput(
        ns("datasetNames"), "Select one or more datasets to process", 
        choices = c(), multiple = TRUE
      )
    
    output$selectionSpace <- renderUI(element)
  })
  
  # --------------- PROCESS DATA ---------------
  
  observeEvent(input$doProcess, {
    
    datasetNames <- rv$datasetNames
    req(datasetNames)
    
    # When doing consecutive runs, the help message should "reappear" for each run
    output$helpMessage <- renderText("")
    Sys.sleep(1)
    
    tryCatch({
      rv$processedData <- processDatasetsWithPiccr(datasetNames, input, project())
      
      saveProcessedDataOnServer(rv$processedData, project())
      outputProcessedData(rv$processedData, serverEnvironment)
      outputNInj(input$averageOverInj, serverEnvironment)
      signalSuccess(serverEnvironment)
      
      output$helpMessage <- renderText(
        "Data processed successfully. Processed data saved on server.")
      
    }, error = function(errorMessage) {
      output$helpMessage <- renderText(
        "An error occured and the data could not be processed. See the logs for details.")
      flog.error(errorMessage)
    })
  })
  
  # ------------- DOWNLOAD PROCESSED DATA -------------
  
  output$download <- downloadHandler(
    filename = "processed.zip",
    content = function(file) {
      downloadProcessedData(file, rv$processedData)
    }
  )
  
}

########################
# HELPERS
########################

signalSuccess <- function(serverEnvironment){
  evalq(rv$processingSuccessful <- TRUE, envir = serverEnvironment)
}

outputProcessedData <- function(processedData, serverEnvironment){
  assign(".processedData", processedData, envir = serverEnvironment)
  evalq(
    rv$processedData <- .processedData, envir = serverEnvironment
  )
}

outputNInj <- function(nInj, serverEnvironment){
  assign(".nInj", nInj, envir = serverEnvironment)
  evalq(
    rv$nInj <- .nInj, envir = serverEnvironment
  )
}

downloadProcessedData <- function(file, processedData){
  
  flog.info("Downloading data")
  
  #go to a temp dir to avoid permission issues
  owd <- setwd(tempdir())
  on.exit(setwd(owd))
  
  flog.debug("Writing files")
  walk(processedData, ~ write_csv(.$processed, .$name))
  
  filenames <- map_chr(processedData, ~ .$name)
  flog.debug(str_c("Filenames: ", paste(filenames, collapse = ", ")))
  
  flog.debug("Creating zip archive")
  # zip does not override existing files
  file.remove(file)
  zip(file, filenames)
}

#' @param dateRange Vector of Date objects. Length two. 
getDatasetsInDateRange <- function(dateRange, project, basePath = BASE_PATH){
  
  req(dateRange)
  
  dateInterval <- interval(first(dateRange), last(dateRange))
  projectData <- loadProjectData(project, basePath)
  datasetNames <- map(names(projectData), ~ {
    date <- ymd(projectData[[.]]$date)
    if (date %within% dateInterval) return(.)
  })
  datasetNames <- list.filter(datasetNames, ~ !is.null(.))
  datasetNames <- as.character(datasetNames)
  
  return(datasetNames)
} 

getNamesOfDatasetsInProject <- function(project, basePath = BASE_PATH){
  
  list.dirs(file.path(basePath, project, "data"), recursive = FALSE, full.names = FALSE)
}

processDatasetsWithPiccr <- function(datasetNames, input, project){
  
  datasets <- loadSelectedDatasets(datasetNames, project)
  processingOptions <- loadSelectedProcessingOptions(datasetNames, project)
  
  # input$driftAndCalibration is in the form "X/Y" where X is the calibration flag 
  # and Y indicates whether to use three-point calibration or not
  calibrationFlag <- as.numeric(str_split(input$driftAndCalibration, "/")[[1]][[1]])
  useThreePointCalibration <- as.logical(str_split(input$driftAndCalibration, "/")[[1]][[2]])
  
  processedData <- processDataWithPiccr(
    datasets, processingOptions, as.logical(input$useMemoryCorrection), 
    calibrationFlag, useThreePointCalibration, input$averageOverInj
  )
  return(processedData)
}

saveProcessedDataOnServer <- function(processedData, project, basePath = BASE_PATH){
  
  walk(processedData, ~ {
    outputDir <- file.path(basePath, project, "data", .$name)
    dir.create(outputDir)
    write_csv(.$processed, file.path(outputDir, "processed.csv"))
  })
}

loadSelectedDatasets <- function(selected, project, basePath = BASE_PATH){
  
  datasets <- map(selected, function(selected){
    rawDataPath <- getPathToRawData(selected, project, basePath)
    read_csv(rawDataPath)
  })
  names(datasets) <- selected
  return(datasets)
}

loadSelectedProcessingOptions <- function(selected, project, basePath = BASE_PATH){
  processingOptions <- map(selected, function(selected){
    path <- file.path(basePath, project, "data", selected, "processingOptions.csv")
    read_csv(path)
  })
  names(processingOptions) <- selected
  return(processingOptions)
}

#' Get the smallest number of injections for any sample in the selected datasets
getMinInjectionCount <- function(datasetNames, project, basePath = BASE_PATH){
  
  datasets <- loadSelectedDatasets(datasetNames, project)
  minInjCountsSelectedDatasets <- map_dbl(datasets, ~ {
    vec <- .$`Inj Nr`
    min(c(vec[vec > lead(vec)], last(vec)), na.rm = T)
  })
  minInjCount <- min(minInjCountsSelectedDatasets, na.rm = TRUE)
  return(minInjCount)
}