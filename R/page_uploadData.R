library(shiny)

pageUploadDataUI <- function(id){
  
  # create namespace function
  ns <- NS(id)
  
  tagList(
    h2("Upload measurement data"), br(),
    
    wellPanel(
      fileInput(ns("file"), "Select a file to upload"),
      textInput(ns("name"), "Name the dataset"),
      textAreaInput(ns("info"), "Information about the dataset (optional)"), br(),
      actionButton(ns("upload"), "Upload the dataset", style = blue),
      textOutput(ns("helpMessage"))
    )
  )
}

pageUploadData <- function(input, output, session, project){
  
  observeEvent(input$upload, {
    message <- uploadDataset(input, project)
    output$helpMessage <- renderText(message)
  })
}

uploadDataset <- function(input, project, basePath = BASE_PATH){
  
  file <- input$file
  name <- input$name
  
  if (is.null(file)) return("You need to upload a file before clicking this button.")
  if (is.null(name)) return("You need to name the dataset before clicking this button.")
  
  fileName <- file$name
  filePath <- file$datapath
  
  data <- read_csv(filePath)
  
  outputDir <- file.path(basePath, project, "data", name)
  dir.create(outputDir, recursive = TRUE)
  write_csv(data, file.path(outputDir, fileName))
  
  firstIdentifier2 <- first(data$`Identifier 2`)
  uniqueIdentifier <- str_extract(firstIdentifier2, "(?<=_).+$")
  optionsPath <- file.path(basePath, "processingOptions", uniqueIdentifier)
  processingOptions <- read_csv(file.path(optionsPath, "processingOptions.csv"))
  sampleDescription <- read_csv(file.path(optionsPath, "sampleDescription.csv"))
  
  write_csv(processingOptions, file.path(outputDir, "processingOptions.csv"))
  write_csv(sampleDescription, file.path(outputDir, "sampleDescription.csv"))
  
  return("Dataset sucessfully uploaded.")
  
}