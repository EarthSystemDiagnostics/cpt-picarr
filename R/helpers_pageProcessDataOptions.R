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
                   "Apply two-point calibration without drift correction",
                   "Apply three-point calibration without drift correction",
                   "Apply two-point calibration and linear drift correction",
                   "Apply three-point calibration and linear drift correction",
                   "Apply double two-point calibration",
                   "Apply double three-point calibration"),
                 choiceValues = list(
                   # "x/y": x is the calibration flag, y indicates if three-point calibration is to be used.
                   "0/F", "0/T", "1/F", "1/T", "2/F", "2/T"
                 )),
    radioButtons(ns("accumulation"), "How should the injections be accumulated?", 
                 choices = list("Average over all injections" = "all", "Average over last n injections" = "last",
                                "Average over a specific range of injections" = "range"),
                 selected = NA),
    uiOutput(ns("accumulationSpace")),
    h4("All set up?"), br(),
    actionButton(ns("doProcess"), "Process the data", style = blue),
    downloadButton(ns("download"), "Download the processed data"),
    textOutput(ns("helpMessage"))
  )
  
}

pageProcessDataOptions <- function(input, output, session, id, projectDataChanged, project, serverEnvironment){
  
  # ---------------- CREATE OWN REACTIVE VALUES ---------
  
  rv <- reactiveValues()
  rv$datasetNames <- c()
  rv$averageOverInj <- NULL
  
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
  
  # keep rv$averageOverInj up to date
  observeEvent(input$averageOverInj, rv$averageOverInj <- input$averageOverInj)
  observeEvent(input$averageRange, rv$averageOverInj <- sprintf("%i:%i", input$averageRange[[1]], input$averageRange[[2]]))
  
  # keep rv$nInj up to date in calling environment
  observeEvent(rv$averageOverInj, outputNInj(rv$averageOverInj, serverEnvironment))
  
  # update possible choices for number of injections to average over
  observeEvent(rv$datasetNames, {
    req(rv$datasetNames)
    minInjCount <- getMinInjectionCount(rv$datasetNames, project())
    updateSelectInput(session, "averageOverInj", choices = 1:minInjCount)
  })
  
  # --------------- RENDER DATASET SELECTION SECTION -------------
  
  observeEvent(input$selectionType, {
    
    rv$datasetNames <- c()
    
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
  
  # -------------- RENDER ACCUMULATION OPTIONS SECTION -----
  
  observeEvent(input$accumulation, {
    
    if (input$accumulation == "all"){
      output$accumulationSpace <- renderUI({})
      rv$averageOverInj <- "all"
    } else if (input$accumulation == "last"){
      output$accumulationSpace <- renderUI({
        selectizeInput(ns("averageOverInj"), 
                       "Average over the last n injections (selecting datasets updates choices)", 
                       choices = 1:getMinInjectionCount(rv$datasetNames, project()))
      })
    } else if (input$accumulation == "range")
      output$accumulationSpace <- renderUI({
        sliderInput(ns("averageRange"), label = "Average over a specific range of injections (selecting datasets updates choices)", 
                    min = 1, max = getMinInjectionCount(rv$datasetNames, project()), value = c(min, max))
      })
  })
  
  # --------------- PROCESS DATA ---------------
  
  observeEvent(input$doProcess, {
    
    datasetNames <- rv$datasetNames
    req(datasetNames)
    
    # When doing consecutive runs, the help message should "reappear" for each run
    output$helpMessage <- renderText("")
    Sys.sleep(1)
    
    tryCatch({
      rv$processedData <- processDatasetsWithPiccr(datasetNames, input, rv$averageOverInj, project())
      
      saveProcessedDataOnServer(rv$processedData, project())
      outputProcessedData(rv$processedData, serverEnvironment)
      signalSuccess(serverEnvironment)
      # signal to other modules that the project data has been changed
      envir <- get("serverEnvironment", envir = serverEnvironment)
      evalq(rv$projectDataChanged <- rv$projectDataChanged + 1, envir = envir)
      
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

processDatasetsWithPiccr <- function(datasetNames, input, averageOverInj, project){
  
  datasets <- loadSelectedDatasets(datasetNames, project)
  processingOptions <- loadSelectedProcessingOptions(datasetNames, project)
  
  # input$driftAndCalibration is in the form "X/Y" where X is the calibration flag 
  # and Y indicates whether to use three-point calibration or not
  calibrationFlag <- as.numeric(str_split(input$driftAndCalibration, "/")[[1]][[1]])
  useThreePointCalibration <- as.logical(str_split(input$driftAndCalibration, "/")[[1]][[2]])
  
  print(averageOverInj)
  
  processedData <- processDataWithPiccr(
    datasets, processingOptions, as.logical(input$useMemoryCorrection), 
    calibrationFlag, useThreePointCalibration, averageOverInj
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
  
  if (is.null(datasetNames)) return(1)
  
  datasets <- loadSelectedDatasets(datasetNames, project)
  minInjCountsSelectedDatasets <- map_dbl(datasets, ~ {
    vec <- .$`Inj Nr`
    min(c(vec[vec > lead(vec)], last(vec)), na.rm = T)
  })
  minInjCount <- min(minInjCountsSelectedDatasets, na.rm = TRUE)
  return(minInjCount)
}