library(shiny)

#' pageUploadDataUI
#' 
#' UI function for the page 'Upload measurement data'
#'
#' @param id Identifier for the namespace of this module
#'
#' @return A HTML tag object
pageUploadDataUI <- function(id){
  
  # create namespace function
  ns <- NS(id)
  
  tagList(
    h2("Upload measurement data"), 
    textOutput(ns("projectName")), br(),
    
    wellPanel(
      fileInput(ns("file"), "Select a file to upload"),
      textInput(ns("name"), "Name the dataset"),
      selectInput(ns("device"), "What device was this data measured with?", choices = c()),
      textAreaInput(ns("info"), "Information about the dataset (optional)"), br(),
      actionButton(ns("upload"), "Upload the dataset", style = blue),
      textOutput(ns("helpMessage"))
    ),
    
    actionButton(ns("goToPageProject"), "Back to page 'Project'")
  )
}

#' pageUploadData
#'
#' Implements the server logic for the page 'Upload measurement data'.
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
#'
#' @return No explicit return value
pageUploadData <- function(input, output, session, project, serverEnvironment){
  
  # display currently loaded project
  output$projectName <- renderText(sprintf("Project: %s", project()))
  
  # update list of possible devices
  updateSelectInput(
    session, "device", choices = map(getDevices(), ~ sprintf("%s (%s)", .$name, .$code)))
  
  # auto fill in name field when file is uploaded
  observeEvent(input$file, {
    updateTextInput(session, "name", value = input$file$name)
  })
  
  observeEvent(input$upload, {
    message <- uploadDataset(input, project())
    output$helpMessage <- renderText(message)
    
    # signal to other modules that the project data has been changed
    evalq(rv$projectDataChanged <- rv$projectDataChanged + 1, envir = serverEnvironment)
  })
  
  observeEvent(input$goToPageProject, goToPage("Project", serverEnvironment))
}

##################
# HELPERS
##################

uploadDataset <- function(input, project, basePath = BASE_PATH){
  
  file <- input$file
  name <- input$name
  device <- input$device
  
  # exit early if required fields are missing
  if (!isTruthy(file)) return("You need to upload a file before clicking this button.")
  if (!isTruthy(name)) return("You need to name the dataset before clicking this button.")
  if (!isTruthy(device)) return("You need to select a device before clicking this button.")
  
  fileName <- file$name
  filePath <- file$datapath
  
  data <- read_csv(filePath)
  uniqueIdentifier <- getUniqueIdentifer(data)
  optionsPath <- file.path(basePath, "processingOptions", uniqueIdentifier)
  
  # exit early if unique identifier can't be found or no processing options are known for the unique identifier 
  if (!isTruthy(uniqueIdentifier)) 
    return("Error: No unique identifier found in the uploaded dataset.")
  if (dataIsMissing(optionsPath))
    return(sprintf("Error: Could not find processing options for the uploaded dataset. (unique id: %s)", uniqueIdentifier))
  
  processingOptions <- read_csv(file.path(optionsPath, "processingOptions.csv"))
  sampleDescription <- read_csv(file.path(optionsPath, "sampleDescription.csv"))
  
  outputDir <- file.path(basePath, project, "data", name)
  
  # exit early if the dataset name is already in use
  if (dir.exists(outputDir))
    return(sprintf("Upload aborted. A dataset with the same name exists already. (path: %s)", outputDir))
  
  saveData(outputDir, data, fileName, processingOptions, sampleDescription)
  createInfoFile(outputDir, data, device, input$info)
  
  return(sprintf("Dataset sucessfully uploaded. Processing Options and 
                 sample descriptions were found. (The data is in %s)", outputDir))
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

createInfoFile <- function(outputDir, data, device, info){
  
  list.save(
    list(date = getDate(data), device = device, additionalInfo = info),
    file.path(outputDir, "fileInfo.json")
  )
}

getDate <- function(data){
  lubridate::ymd_hms(data$`Time Code`) %>%
    lubridate::date() %>%
    first() %>%
    as.character()
}
