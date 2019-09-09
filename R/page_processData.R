library(tidyverse)
library(rhandsontable)
library(piccr)
library(futile.logger)

#' pageProcessDataUI
#' 
#' UI function for the page 'Process measurement data'
#'
#' @param id Identifier for the namespace of this module
#'
#' @return A HTML tag object
pageProcessDataUI <- function(id){
  
  # create namespace function
  ns <- NS(id)
  
  tagList(
    h2("Process isotope measurement data"),
    
    wellPanel(
      h4("Setup and Options"), br(),
      selectizeInput(ns("datasetNames"), "Select one or more datasets to process", choices = c(), multiple = TRUE),
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
    ),
    
    wellPanel(
      h4("Plots and tables"),
      uiOutput(ns("plots"))
    ),
    
    actionButton(ns("goToPageProject"), "Back to page 'Project'"),
    
    # silently pass the given id to the server function
    conditionalPanel("false", textInput(ns("id"), label = "", value = id))
  )
}

#' pageProcessData
#'
#' Implements the server logic for the page 'Process measurement data'.
#'
#' @param input Shiny inputs
#' @param output Shiny outputs
#' @param session Shiny session
#' @param project A reactive expression. 'project()' evaluates to a 
#'                String -> the name of the currently loaded project
#' @param serverEnvironment An environment. The environment of the 
#'                          server function that calls this module.
#'                          Used to execute code in the environment of the 
#'                          main server function for the app (e.g. to 
#'                          switch between pages).
#' @param projectDataChanged A reactive expression. Used to trigger an update of 
#'                           the displayed project data when the project data changes. 
#'
#' @return No explicit return value
pageProcessData <- function(input, output, session, project, serverEnvironment, projectDataChanged){
  
  # ---------------- INITIALIZE STATE -------------
  
  rv <- reactiveValues()
  rv$processedData <- NULL  # type: nested list (as output by processDatasetsWithPiccr)
  rv$processingSuccessful <- FALSE  # type: logical
  rv$datasetForPlottingRaw <- NULL  # type: data.frame
  rv$datasetForPlottingProcessed <- NULL  # type: dat.frame
  
  # create a namespace function using the id passed by the frontend
  ns <- reactive(NS(isolate(input$id)))
  
  # --------------- MANAGE STATE -------------
  
  # update the list of selectable datasets when a new 
  # project is loaded or a dataset is uploaded.
  observe({
    
    # add dependency on the reactive expression projectDataChanged
    force(projectDataChanged()) 
    
    datasets <- getNamesOfDatasetsInProject(project())
    updateSelectizeInput(session, "datasetNames", choices = datasets)
    flog.debug("updated dataset selection on page 'process data' (project : %s)", project())
  })
  
  # update rv$datasetForPlotting when a dataset is selected
  observeEvent(input$datasetForPlotting, {
    # update raw data
    path <- getPathToRawData(input$datasetForPlotting, project())
    rv$datasetForPlottingRaw <- read_csv(path)
    flog.debug("update rv$datasetForPlottingRaw. new dataset: %s", path)
    
    # update processed data
    rv$datasetForPlottingProcessed <- rv$processedData[[input$datasetForPlotting]]$processed$data
    flog.debug("update rv$datasetForPlottingProcessed")
  })
  
  # --------------- RENDER PLOTS SECTION DYNAMICALLY --------------
  
  output$plots <- renderUI({
    
    if (!rv$processingSuccessful){
      textOutput(ns()("plotsInfoMessage"))
    } else {
      tagList(
        selectInput(ns()("datasetForPlotting"), "Which dataset would you like to plot?", choices = names(rv$processedData)),
        actionButton(ns()("plotWaterLevel"), "water level"),
        actionButton(ns()("plotStdDev"), "standard deviation"),
        actionButton(ns()("tableRawData"), "raw data (as table)"),
        actionButton(ns()("plotProcessedData"), "processed data"),
        actionButton(ns()("plotMemory"), "memory"),
        p("Not yet implemented:"),
        actionButton(ns()("plotDeviationForStandards"), "deviation from true value for standards"),
        actionButton(ns()("plotRawData"), "raw data (as plot)"),
        actionButton(ns()("plotRawVsProcessed"), "raw vs. processed"),
        actionButton(ns()("plotCalibration"), "calibration"),
        actionButton(ns()("plotDrift"), "drift"),
        hr(),
        uiOutput(ns()("plotOutput"))
      )
    }
  })
  
  # --------------- REACT TO USER INPUT (PROCESSING AND DOWNLOAD) -------------------
  
  observeEvent(input$doProcess, {
    
    datasetNames <- input$datasetNames
    req(datasetNames)
    
    tryCatch({
        
      rv$processedData <- processDatasetsWithPiccr(datasetNames, input, project())
      rv$processingSuccessful <- TRUE
  
      saveProcessedDataOnServer(rv$processedData, project())
      
      output$helpMessage <- renderText("Data processed successfully. Processed data saved on server.")
    
    }, error = function(errorMessage) {
      
      output$helpMessage <- renderText("An error occured and the data could not be processed. 
                                       See the logs for details.")
      flog.error(errorMessage)
    })
  })
  
  output$download <- downloadHandler(
    filename = "processed.zip",
    content = function(file) {
      processedData <- rv$processedData
      downloadProcessedData(file, processedData)
    }
  )
  
  observeEvent(input$goToPageProject,goToTab("Project", session, serverEnvironment))
  
  # --------------- PLOTS ---------------------
  
  output$plotsInfoMessage <- renderText("This section will contain plots and tables once you have processed some data.")
  
  # plot water level
  observeEvent(input$plotWaterLevel, {
    # ---- ui ----
    output$plotOutput <- renderUI(plotOutput(ns()("plot")))
    
    # ---- server ----
    output$plot <- renderPlot({
      ggplot(rv$datasetForPlottingRaw, mapping = aes(x = Line, y = H2O_Mean)) + 
        geom_point()
    })
  })
  
  # plot standard deviation
  observeEvent(input$plotStdDev, {
    # ---- ui ----
    output$plotOutput <- renderUI(plotOutput(ns()("plot")))
    
    # ---- server ----
    data <- rv$datasetForPlottingProcessed
    
    # create y-axis labels
    data <- data %>% 
      mutate(block = str_c(".block", block)) %>%
      mutate(block = replace_na(block, "")) %>%
      mutate(label = str_c(`Identifier 1`, block))
    
    # transform sd data, so that d18O and dD can be plotted in the same plot
    data <- data %>%
      gather("type", "sd", sd.O18, sd.H2)
    
    output$plot <- renderPlot({
      ggplot(data, mapping = aes(x = sd, y = label, color = type)) + 
        geom_point()
    })
  })
  
  # plot raw measurement data
  observeEvent(input$tableRawData, {
    # ---- ui ----
    output$plotOutput <- renderUI(rHandsontableOutput(ns()("table")))
    
    # ---- server ----
    output$table <- renderRHandsontable(rhandsontable(rv$datasetForPlottingRaw))
  })
  
  # plot processed measurement data
  observeEvent(input$plotProcessedData, {
    # ---- ui ----
    output$plotOutput <- renderUI(rHandsontableOutput(ns()("table")))
    
    # ---- server ----
    data <- rv$datasetForPlottingProcessed
    
    # don't include identifer that was appended to column `Identifier 2`
    data <- mutate(data, `Identifier 2` = str_replace(`Identifier 2`, "_.+$", ""))
    
    output$table <- renderRHandsontable(rhandsontable(data))
  })
  
  # plot memory coefficients
  observeEvent(input$plotMemory, {
    # ---- ui ----
    output$plotOutput <- renderUI({
      tagList(
        plotOutput(ns()("plotMemCoeff")),
        br(),
        plotOutput(ns()("plotCompareD18O")),
        br(),
        plotOutput(ns()("plotCompareDD"))
      )
    })
    
    # ---- server ----
    datasetForPlotting <- input$datasetForPlotting
    memoryCorrected <- rv$processedData[[datasetForPlotting]]$memoryCorrected$data
    
    # gather and pre-process memory coefficient data
    memoryCoefficients <- memoryCorrected$memoryCoefficients
    memoryCoefficients <- memoryCoefficients %>%
      rename(O18 = memoryCoeffD18O, H2 = memoryCoeffDD) %>%
      gather("type", "coeff", O18, H2)
    # plot memory coefficients
    output$plotMemCoeff <- renderPlot({
      ggplot(memoryCoefficients, mapping = aes(x = `Inj Nr`, y = coeff, color = type)) + 
        geom_point() + 
        geom_line() + 
        labs(title = "Memory coefficients", x = "Injection Nr.", y = "memory coefficient") + 
        scale_x_continuous(breaks = unique(memoryCoefficients$`Inj Nr`))
    })
    
    # gather and pre-process measurement data for block 1 standards 
    memoryCorrectedStandards <- memoryCorrected$datasetMemoryCorrected %>%
      mutate(type = "memory corrected") %>%
      filter(block == 1)
    rawStandards <- rv$datasetForPlottingRaw %>% 
      mutate(type = "raw") %>%
      filter(Line %in% memoryCorrectedStandards$Line)
    mergedData <- bind_rows(rawStandards, memoryCorrectedStandards)
    # plot raw vs. memory corrected (d18O and dD)
    output$plotCompareD18O <- renderPlot({
      ggplot(mergedData) +
        geom_point(mapping = aes(x = `Inj Nr`, y = `d(18_16)Mean`, color = type)) +
        geom_path(mapping = aes(x = `Inj Nr`, y = `d(18_16)Mean`, color = type)) +
        facet_grid(`Identifier 1` ~ ., scales = "free") +
        labs(title = "Raw and memory corrected for block 1 standards (O18)")
    })
    output$plotCompareDD <- renderPlot({
      ggplot(mergedData) +
        geom_point(mapping = aes(x = `Inj Nr`, y = `d(D_H)Mean`, color = type)) +
        geom_path(mapping = aes(x = `Inj Nr`, y = `d(D_H)Mean`, color = type)) +
        facet_grid(`Identifier 1` ~ ., scales = "free") +
        labs(title = "Raw and memory corrected for block 1 standards (H2)")
    })
    
  })
}


#######################
# HELPERS
#######################

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
  
  processedData <- processDataWithPiccr(datasets, processingOptions, input$useMemoryCorrection, 
                                        calibrationFlag, useThreePointCalibration)
  return(processedData)
}

saveProcessedDataOnServer <- function(processedData, project, basePath = BASE_PATH){
  
  datasetNames <- names(processedData)
  walk(datasetNames, function(datasetName){
    processedData <- processedData[[datasetName]]$processed$data
    outputDir <- file.path(basePath, project, "data", datasetName)
    dir.create(outputDir)
    write_csv(processedData, file.path(outputDir, "processed.csv"))
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
