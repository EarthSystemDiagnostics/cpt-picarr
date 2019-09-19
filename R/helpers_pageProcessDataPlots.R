library(shiny)
library(tidyverse)
library(rlist)
library(rhandsontable)

pageProcessDataPlotsUI <- function(id){
  
  # create namespace function
  ns <- NS(id)
  
  tagList(
    
    selectInput(ns("datasetForPlotting"), "Which dataset do you want to look at?", choices = c()),
    hr(),
    
    fluidRow(
      column(3,
        p(strong("Data")),
        actionButton(ns("dataRaw"), "Raw"),
        actionButton(ns("dataMemoryCorrected"), "Memory corrected"),
        actionButton(ns("dataCalibrated"), "Calibrated"),
        actionButton(ns("dataDrift"), "Calibrated and drift corrected"),
        actionButton(ns("dataProcessed"), "Processed")
      ),
      column(3,
        p(strong("Injection stats")),
        actionButton(ns("injWaterLevel"), "Water level"),
        actionButton(ns("injStdDev"), "Standard deviation")
      ),
      column(3, 
        p(strong("Sample stats")),
        actionButton(ns("sampleWaterLevel"), "Water level"),
        actionButton(ns("sampleStdDev"), "Standard deviation"),
        actionButton(ns("sampleDevFromTrue"), "Deviation from true value (for standards)")
      ),
      column(3,
        p(strong("File stats")),
        actionButton(ns("fileGeneralStats"), "General stats"),
        actionButton(ns("fileMemoryCorrection"), "Memory correction"),
        actionButton(ns("fileCalibration"), "Calibration"),
        actionButton(ns("fileDriftCorrection"), "Drift Correction")
      )
    ),
    
    hr(),
    
    uiOutput(ns("plotOutput"))
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
  
  # ------------------- PLOTS AND TABLES -------------------
  
  # ------------- DATA --------------
  
  observeEvent(input$dataRaw, {
    description <- p(strong("Raw data: "), "The original measurement data before any processing was done.")
    data <- rv$dataToPlot$raw
    dataOutput(output, data = data, description = description, ns = ns)
  })
  observeEvent(input$dataMemoryCorrected, {
    description <- p(strong("Memory corrected data: "), "The data after applying the memory correction.")
    data <- rv$dataToPlot$memoryCorrected
    dataOutput(output, data = data, description = description, ns = ns)
  })
  observeEvent(input$dataCalibrated, {
    description <- p(strong("Calibrated data: "), "The data after applying only a calibration using first-block standards.")
    data <- rv$dataToPlot$calibrated
    dataOutput(output, data = data, description = description, ns = ns)
  })
  observeEvent(input$dataDrift, {
    description <- p(strong("Calibrated and drift corrected data: "), 
                     "The data after applying a linear drift correction and a calibration using", 
                     "first-block standards, or after applying a double calibration (with inherent drift correction).")
    data <- rv$dataToPlot$calibratedAndDriftCorrected
    dataOutput(output, data = data, description = description, ns = ns)
  })
  observeEvent(input$dataProcessed, {
    output$plotOutput  <- renderDataUI(ns)
    
    description <- p(strong("Processed data: "), "The final data from averaging across n injections.")
    data <- rv$dataToPlot$processed
    output$description <- renderUI(description)
    output$plotD18O    <- renderPlot(ggplot(data, mapping = aes(Line, `delta.O18`)) + geom_point() + ggtitle("delta O-18"))
    output$plotDD      <- renderPlot(ggplot(data, mapping = aes(Line, `delta.H2`)) + geom_point() + ggtitle("delta H2"))
    output$table       <- renderRHandsontable(rhandsontable(data, height = 500))
  })
  
  
  # ------------- INJECTION-LEVEL STATS ---------
  
  observeEvent(input$injWaterLevel, {
    output$plotOutput <- renderUI({
      tagList(
        p(strong("Water level: "), "Water vapour level (mean and standard deviation) during the injection."), br(),
        plotOutput(ns("plotMean")), br(),
        plotOutput(ns("plotStdDev")), br()
      )
    })
    
    data <- rv$dataToPlot$raw
    output$plotMean <- renderPlot(ggplot(data, mapping = aes(Line, H2O_Mean)) + geom_point() + ggtitle("Mean water level"))
    output$plotStdDev <- renderPlot(ggplot(data, mapping = aes(Line, H2O_SD)) + geom_point() + ggtitle("Std dev of water level"))
  })
  
  observeEvent(input$injStdDev, {
    output$plotOutput <- renderUI({
      tagList(
        p(strong("Standard deviation: "), "Standard deviation of the injection value for each measured",
          "isotopic species from the measurement integration time in the cavity."), br(),
        plotOutput(ns("plotD18O")), br(),
        plotOutput(ns("plotDD"))
      )
    })
    
    data <- rv$dataToPlot$raw
    output$plotD18O <- renderPlot(ggplot(data, mapping = aes(Line, `d(18_16)_SD`)) + geom_point() + ggtitle("Std dev for delta O-18"))
    output$plotDD <- renderPlot(ggplot(data, mapping = aes(Line, `d(D_H)_SD`)) + geom_point() + ggtitle("Std dev for delta H2"))
  })
  
  # --------- SAMPLE-LEVEL STATS ------------
  
  observeEvent(input$sampleWaterLevel, {
    # TODO
  })
  
  observeEvent(input$sampleStdDev, {
    output$plotOutput <- renderUI({
      tagList(
        p(strong("Standard deviation: "), "The standard deviation of the mean isotope value 
          for each isotopic species of this sample/standard."), br(),
        plotOutput(ns("plotD18O")), br(),
        plotOutput(ns("plotDD"))
      )
    })
    
    data <- rv$dataToPlot$processed
    output$plotD18O <- renderPlot(ggplot(data, mapping = aes(Line, sd.O18)) + geom_point() + ggtitle("Std dev for delta O-18"))
    output$plotDD <- renderPlot(ggplot(data, mapping = aes(Line, sd.H2)) + geom_point() + ggtitle("Std dev for delta H2"))
  })
  
  observeEvent(input$sampleDevFromTrue, {
    output$plotOutput <- renderUI({
      tagList(
        p(strong("Deviation from true value: "), "The deviation from the true isotope value (only for standards)."), br(),
        rHandsontableOutput(ns("table"))
      )
    })
    
    data <- rv$dataToPlot$deviationsFromTrue
    output$table <- renderRHandsontable(rhandsontable(data))
  })
  
  # -------------- FILE-LEVEL STATS ---------------
  
  observeEvent(input$fileGeneralStats, {
    output$plotOutput <- renderUI({
      tagList(
        p(strong("General stats")), br(),
        p(strong("Deviation of quality control standard: "), "The specific deviation", 
          "from the true value for the independent quality control standard."),
        rHandsontableOutput(ns("table1")), br(),
        p(strong("rmsd of deviations: "), "The root mean square deviation across the ",
          "deviations of all measured standards from their true values."), br(),
        rHandsontableOutput(ns("table2")), br(),
        p(strong("Pooled standard deviation: "), "The pooled standard deviation ",
          "across all measured samples and standards."),
        rHandsontableOutput(ns("table3"))
      )
    })
    output$table1 <- renderRHandsontable(rhandsontable(data.frame(rv$dataToPlot$deviationOfControlStandard)))
    output$table2 <- renderRHandsontable(rhandsontable(data.frame(rv$dataToPlot$rmsdDeviationsFromTrue)))
    output$table3 <- renderRHandsontable(rhandsontable(data.frame(rv$dataToPlot$pooledStdDev)))
  })
  
  observeEvent(input$fileMemoryCorrection, {
    # TODO
  })
  
  observeEvent(input$fileCalibration, {
    # TODO
  })
  
  observeEvent(input$fileDriftCorrection, {
    # TODO
  })
}

########################
# HELPERS
########################

dataOutput <- function(output, data, description, ns){
  
  output$plotOutput  <- renderDataUI(ns)
  
  output$description <- renderUI(description)
  output$plotD18O    <- renderPlot(ggplot(data, mapping = aes(Line, `d(18_16)Mean`)) + geom_point() + ggtitle("delta O-18"))
  output$plotDD      <- renderPlot(ggplot(data, mapping = aes(Line, `d(D_H)Mean`)) + geom_point() + ggtitle("delta H2"))
  output$table       <- renderRHandsontable(rhandsontable(data, height = 500))
}

renderDataUI <- function(ns){
  renderUI({
    tagList(
      uiOutput(ns("description")), br(),
      plotOutput(ns("plotD18O")), br(),
      plotOutput(ns("plotDD")), br(),
      rHandsontableOutput(ns("table"))
    )
  })
}