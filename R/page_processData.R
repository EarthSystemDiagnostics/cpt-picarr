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
    ),
    
    wellPanel(
      h4("Plots and tables"),
      uiOutput(ns("plots"))
    ),
    
    # silently pass id to backend
    conditionalPanel("false", textInput(ns("id"), label = "", value = id))
  )
}

pageProcessData <- function(input, output, session){
  
  rv <- reactiveValues()
  rv$rawData <- NULL
  rv$processedData <- NULL
  rv$processingSuccessful <- FALSE
  
  # create a namespace function using the id passed by the frontend
  ns <- reactive(NS(isolate(input$id)))
  
  observeEvent(input$doProcess, {
    req(input$files)
    tryCatch({
        rv$processedData <- processDataWithPiccr(input, BASE_PATH)
        rv$processingSuccessful <- TRUE
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
      processedData <- rv$processedData
      downloadProcessedData(file, processedData)
    }
  )
  
  output$plots <- renderUI({
    
    ns <- ns()
    
    if (!rv$processingSuccessful){
      textOutput(ns("plotsInfoMessage"))
    } else {
      tagList(
        selectInput(ns("datasetForPlotting"), "Which dataset would you like to plot?", choices = input$files$name),
        actionButton(ns("plotWaterLevel"), "water level"),
        actionButton(ns("plotStdDev"), "standard deviation"),
        actionButton(ns("plotDeviationForStandards"), "deviation from true value for standards"),
        actionButton(ns("plotRawData"), "raw data"),
        actionButton(ns("plotProcessedData"), "processed data"),
        actionButton(ns("plotRawVsProcessed"), "raw vs. processed"),
        actionButton(ns("plotMemory"), "memory coefficients"),
        actionButton(ns("plotCalibration"), "calibration"),
        actionButton(ns("plotDrift"), "drift"),
        hr(),
        uiOutput(ns("plotOutput"))
      )
    }
    
  })

  output$plotsInfoMessage <- renderText("This section will contain plots and tables once you have processed some data.")
  
  # plot water level
  observeEvent(input$plotWaterLevel, {
    # ---- ui ----
    output$plotOutput <- renderUI(plotOutput(ns()("plot")))
    
    # ---- server ----
    data <- getRawData(input$datasetForPlotting, input$files)
    output$plot <- renderPlot({
      ggplot(data, mapping = aes(x = Line, y = H2O_Mean)) + 
        geom_point()
    })
  })
  
  # plot standard deviation
  observeEvent(input$plotStdDev, {
    # ---- ui ----
    output$plotOutput <- renderUI(plotOutput(ns()("plot")))
    
    # ---- server ----
    datasetForPlotting <- input$datasetForPlotting
    data <- rv$processedData[[datasetForPlotting]]$processed$data
    
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
  observeEvent(input$plotRawData, {
    # ---- ui ----
    output$plotOutput <- renderUI(rHandsontableOutput(ns()("table")))
    
    # ---- server ----
    data <- getRawData(input$datasetForPlotting, input$files)
    output$table <- renderRHandsontable(rhandsontable(data))
  })
  
  # plot processed measurement data
  observeEvent(input$plotProcessedData, {
    # ---- ui ----
    output$plotOutput <- renderUI(rHandsontableOutput(ns()("table")))
    
    # ---- server ----
    datasetForPlotting <- input$datasetForPlotting
    data <- rv$processedData[[datasetForPlotting]]$processed$data
    
    # don't include identifer that was appended to column `Identifier 2`
    data <- mutate(data, `Identifier 2` = str_replace(`Identifier 2`, "_.+$", ""))
    
    output$table <- renderRHandsontable(rhandsontable(data))
  })
  
  # plot memory coefficients
  observeEvent(input$plotMemory, {
    # ---- ui ----
    output$plotOutput <- renderUI(plotOutput(ns()("plot")))
    
    # ---- server ----
    datasetForPlotting <- input$datasetForPlotting
    memoryCoefficients <- rv$processedData[[datasetForPlotting]]$memoryCorrected$data$memoryCoefficients
    memoryCoefficients <- memoryCoefficients %>%
      rename(O18 = memoryCoeffD18O, H2 = memoryCoeffDD) %>%
      gather("type", "coeff", O18, H2)
    
    output$plot <- renderPlot({
      ggplot(memoryCoefficients, mapping = aes(x = `Inj Nr`, y = coeff, color = type)) + 
        geom_point() + 
        geom_line() + 
        labs(x = "Injection Nr.", y = "memory coefficient") + 
        scale_x_continuous(breaks = unique(memoryCoefficients$`Inj Nr`))
    })
  })
}


#######################
# HELPERS
#######################

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

getRawData <- function(datasetName, files) {
  datasetIndexInFileList <- which(files$name == datasetName)
  pathToDataset <- files$datapath[[datasetIndexInFileList]]
  data <- read_csv(pathToDataset)
  
  # don't include identifer that was appended to column `Identifier 2`
  data <- mutate(data, `Identifier 2` = str_replace(`Identifier 2`, "_.+$", ""))
  
  return(data)
}