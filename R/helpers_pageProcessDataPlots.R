pageProcessDataPlotsUI <- function(id){
  
  # create namespace function
  ns <- NS(id)
  
  tagList(
    selectInput(ns("datasetForPlotting"), "Which dataset would you like to plot?", choices = c()),
    
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
      choices = map_chr(processedData(), ~ .[[1]]$name)
    )
  })
  
  # update rv$datasetForPlotting when a dataset is selected
  observeEvent(input$datasetForPlotting, {
    name <- input$datasetForPlotting
    rv$dataToPlot <- list.filter(processedData(), ~ name == .[[1]]$name) %>%
      first() %>%
      .[[1]]
    flog.debug("updated dataset to plot")
  })
}