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
    message <- uploadDataset(input, project())
    output$helpMessage <- renderText(message)
  })
}

##################
# HELPERS
##################

uploadDataset <- function(input, project, basePath = BASE_PATH){
  
  file <- input$file
  name <- input$name
  
  if (!isTruthy(file)) return("You need to upload a file before clicking this button.")
  if (!isTruthy(name)) return("You need to name the dataset before clicking this button.")
  
  fileName <- file$name
  filePath <- file$datapath
  
  data <- read_csv(filePath)
  uniqueIdentifier <- getUniqueIdentifer(data)
  
  if (is.na(uniqueIdentifier)) 
    return(sprintf("Error: No unique identifier found in the uploaded dataset. (path: %s)", filePath))
  
  optionsPath <- file.path(basePath, "processingOptions", uniqueIdentifier)
  
  if (dataIsMissing(optionsPath))
    return(sprintf("Error: Could not find processing options for the uploaded dataset. (unique id: %s)", uniqueIdentifier))
  
  processingOptions <- read_csv(file.path(optionsPath, "processingOptions.csv"))
  sampleDescription <- read_csv(file.path(optionsPath, "sampleDescription.csv"))
  
  outputDir <- file.path(basePath, project, "data", name)
  
  if (dir.exists(outputDir))
    return(sprintf("Upload aborted. A dataset with the same name exists already. (path: %s)", outputDir))
  
  saveData(outputDir, data, fileName, processingOptions, sampleDescription)
  saveAdditionalInfo(outputDir, input$info)
  
  return(sprintf("Dataset sucessfully uploaded. (The uploaded data is in %s)", outputDir))
  
}

getUniqueIdentifer <- function(dataset){
  firstIdentifier2 <- first(dataset$`Identifier 2`)
  uniqueIdentifier <- str_extract(firstIdentifier2, "(?<=_).+$")
  return(uniqueIdentifier)
}

dataIsMissing <- function(optionsPath){
  processingOptionsExist <- file.exists(file.path(optionsPath, "processingOptions.csv"))
  sampleDescriptionExists <- file.exists(file.path(optionsPath, "sampleDescription.csv"))
  !processingOptionsExist | !sampleDescriptionExists
}

saveData <- function(outputDir, data, fileName, processingOptions, sampleDescription){
  
  dir.create(outputDir, recursive = TRUE)
  
  dataWithoutIdentifier <- mutate(data, `Identifier 2` = str_replace(`Identifier 2`, "_[^_]+$", ""))
  write_csv(dataWithoutIdentifier, file.path(outputDir, fileName))
  write_csv(processingOptions, file.path(outputDir, "processingOptions.csv"))
  write_csv(sampleDescription, file.path(outputDir, "sampleDescription.csv"))
}

saveAdditionalInfo <- function(outputDir, info){
  path <- file.path(outputDir, "fileInfo.txt")
  write_file(info, path)
}
