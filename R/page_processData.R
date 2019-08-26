source("global.R")

pageProcessDataUI <- function(id){
  
  # create namespace function
  ns <- NS(id)
  
  tagList(
    h2("Process isotope measurement data"),
    p(em("Selected project: Project A")), br(),
    wellPanel(
      h4("Setup and Options"), br(),
      selectInput(ns("data_to_process"), "Select one or more datasets to process", c("Dataset A", "Dataset B", "Dataset C"), multiple = TRUE),
      dateRangeInput(ns("data_to_process_range"), "Process all the data in this timespan (overrides the selection above):"),
      actionButton(ns("change_processing_template"), "Update the processing template for one of the datasets"), p(""), br(),
      radioButtons(ns("use_memory_correction"), "Use memory correction?", c("Yes", "No")),
      radioButtons(ns("calibration_type"), "Use three-point calibration or two-point calibration?", c("Use three-point calibration",
                                                                                               "Use two-point calibration")),
      radioButtons(ns("drift_and_calibration"), "Drift correction and calibration options", c("Use linear drift correction and calibration", 
                                                                                       "Use double calibration",
                                                                                       "Use only calibration, without drift correction")),
      h4("All set up?"), br(),
      actionButton(ns("do_process"), "Process the data", style = blue),
      actionButton(ns("do_download"), "Download the processed data"),
      actionButton(ns("do_save"), "Save the processed data on the server")
    )
  )
}

pageProcessData <- function(input, output, session){
  
}