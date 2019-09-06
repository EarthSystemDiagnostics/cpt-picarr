library(shiny)
library(tidyverse)
library(futile.logger)


#' pageProjectUI
#' 
#' UI function for the page 'Project'
#'
#' @param id Identifier for the namespace of this module
#'
#' @return A HTML tag object
pageProjectUI <- function(id){
  
  # create namespace function
  ns <- NS(id)

  tagList(
    uiOutput(ns("header")),
    wellPanel(
      h3("Project Information"),
      p(strong("Project name:")),
      textOutput(ns("projName")), br(),
      p(strong("People involved:")),
      textOutput(ns("projPeople")), br(),
      p(strong("Additional info:")),
      textOutput(ns("projAdditional")), br(),
      p(strong("Expedition date:")),
      textOutput(ns("projDate"))
    ),

    wellPanel(
      style = "height: 580px",
      h3("Project Data"),
      
      conditionalPanel("true",
        # add CSS to make panel scrollable to reduce vertical size
        style = "overflow-y:scroll; height: 420px",
        uiOutput(ns("projectData"))
      ), 
      br(),
      
      actionButton(ns("downloadAllData"), "Download all project data")
    ),
    wellPanel(
      h3("What do you want to do next?"),
      
      actionButton(ns("goToGenerateSampleDescr"), "Generate a sample description", style = blue),
      actionButton(ns("goToUploadData"), "Upload measurement data", style = blue),
      actionButton(ns("goToProcessData"), "Process measurement data", style = blue)
      
    )
  )
}

#' pageProjecte
#'
#' Implements the server logic for the page 'Project'.
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
pageProject <- function(input, output, session, project, serverEnvironment){
  
  # ------------ NAVIGATION BETWEEN PAGES -----------
  
  observeEvent(
    input$goToGenerateSampleDescr, 
    goToTab("Generate a sample description", session, serverEnvironment)
  )
  observeEvent(
    input$goToProcessData, 
    goToTab("Process measurement data", session, serverEnvironment)
  )
  observeEvent(
    input$goToUploadData, 
    goToTab("Upload measurement data", session, serverEnvironment)
  )
  
  # ------------ RENDER OUTPUT --------------
  
  # output project info
  observe({
    
    projectName <- project()
    req(projectName)
    
    projectInfo           <- loadProjectInfo(projectName)
    output$projName       <- renderText(projectInfo$name)
    output$projPeople     <- renderText(projectInfo$people)
    output$projAdditional <- renderText(projectInfo$additionalInfo)
    output$projDate       <- renderText(projectInfo$date)
  })
  
  
  # display project data
  output$projectData <- renderUI({
    
    projectName <- project()
    req(projectName)
    
    projectData <- loadProjectData(projectName)
    
    # create dynamic number of output ui elements
    uiElementsList <- map2(projectData, names(projectData), ~{
      wellPanel(
        p(strong(.y)),
        # Only output attributes if they are given in projectData
        if(isTruthy(.x$path))           p("path: ", .x$path),
        if(isTruthy(.x$raw))            p("raw data: ", .x$raw),
        if(isTruthy(.x$date))           p("date measured: ", .x$date),
        if(isTruthy(.x$processed))      p("processed data: ", .x$processed),
        if(isTruthy(.x$additionalInfo)) p("additional info: ", .x$additionalInfo)
      )
    })
    tagList(uiElementsList)
  })
  
}

#######################
# HELPERS
#######################

loadProjectInfo <- function(projectName, basePath = BASE_PATH){
  path <- file.path(basePath, projectName, "projectInfo.json")
  flog.debug(sprintf("loading project info: %s", path))
  rlist::list.load(path)
}

#' Get a List with relevant information about the project. 
#' 
#' Relies on getStoredData(..).
#' 
#' @return A named list. The names correspond to the names of the datsets 
#'         for the given project. Has the following components:
#'           $path (path to the folder with data for this dataset)
#'           $raw  (name of the file with raw data)
#'           $date (measurement date)
#'           $additionalInfo (content of the file 'fileInfo.txt')
#'           $processed ('processed.csv' if the dataset has been processed)
#'         Note that only the components are present, for which data
#'         exists.
loadProjectData <- function(project, basePath = BASE_PATH){
  
  projectDataPath <- file.path(basePath, project, "data")
  flog.debug(sprintf("loading data info: %s", projectDataPath))
  
  datasetNames <- list.files(projectDataPath, recursive = FALSE)
  
  projectData <- map(datasetNames, getStoredData, 
                     project = project, basePath = basePath)
  names(projectData) <- datasetNames
  return(projectData)
}

getStoredData <- function(datasetName, project, basePath){
  
  projectDataPath <- file.path(basePath, project, "data")
  
  # use to assemble output step-by-step
  output <- list(
    path = file.path(projectDataPath, datasetName)
  )
  
  # add rawFile and date to output
  rawFile <- getPathToRawData(datasetName, project, basePath, fullPath = FALSE)
  if (!is_empty(rawFile)){
    rawData <- read_csv(file.path(projectDataPath, datasetName, rawFile))
    date <- lubridate::ymd_hms(rawData$TimeCode) %>%
      lubridate::date() %>%
      first() %>%
      as.character()
    output$raw <- rawFile
    output$date <- date
  }
  
  # add additionalInfo to output
  infoFile <- file.path(projectDataPath, datasetName, "fileInfo.txt")
  if(file.exists(infoFile)) output$additionalInfo <- read_file(infoFile)
  
  # add processed to output
  processedFile <- file.path(projectDataPath, datasetName, "processed.csv")
  if(file.exists(processedFile)) output$processed <- "processed.csv"
  
  return(output)
}