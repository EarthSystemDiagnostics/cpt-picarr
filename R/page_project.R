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
      
      downloadButton(ns("downloadAllData"), 
                     "Download all project data"),
      downloadButton(ns("downloadProcessedAsZip"), 
                     "Download all processed data (zip)"),
      downloadButton(ns("downloadProcessedSingleFile"), 
                     "Download all processed data (single file)")
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
#' @param projectDataChanged A reactive expression. Used to trigger an update of 
#'                           the displayed project data when the project data changes. 
#'
#' @return No explicit return value
pageProject <- function(input, output, session, project, serverEnvironment, projectDataChanged){
  
  # ------------ NAVIGATION BETWEEN PAGES -----------
  
  observeEvent(
    input$goToGenerateSampleDescr, 
    goToPage("Generate a sample description", serverEnvironment)
  )
  observeEvent(
    input$goToProcessData, 
    goToPage("Process measurement data", serverEnvironment)
  )
  observeEvent(
    input$goToUploadData, 
    goToPage("Upload measurement data", serverEnvironment)
  )
  
  # ------------ RENDER OUTPUT --------------
  
  # project name is page header
  output$header <- renderUI({
    h2("Project: ", project())
  })
  
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
    
    # make this output depend on the reactive expression projectDataChanged
    force(projectDataChanged()) 
    
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
        if(isTruthy(.x$processed))      p("processed data: ", .x$processed),
        if(isTruthy(.x$date))           p("date measured: ", .x$date),
        if(isTruthy(.x$device))         p("measurement instrument: ", .x$device),
        if(isTruthy(.x$additionalInfo)) p("additional info: ", .x$additionalInfo)
      )
    })
    tagList(uiElementsList)
  })
  
  # ------------- DOWNLOAD DATA -------------------
  
  output$downloadAllData <- downloadHandler(
    filename = sprintf("data.zip", project()),
    content = function(file){
      flog.debug("Downloading all project data. Project: %s", project())
      
      projectDataPath <- file.path(BASE_PATH, project(), "data")
      filesToZip <- list.files(projectDataPath, recursive = TRUE)
      
      # Zip file should contain paths relative to the project data directory
      owd <- setwd(projectDataPath)
      on.exit(setwd(owd))

      zip(file, filesToZip)
    }
  )
  
  output$downloadProcessedAsZip <- downloadHandler(
    filename = "processedData.zip",
    content = function(file){
      flog.debug("Downloading processed data (as zip). Project: %s", project())
      
      downloadProcessedDataAsZip(project(), file)
    }
  )
  
  output$downloadProcessedSingleFile <- downloadHandler(
    filename = "processedData.csv",
    content = function(file){
      flog.debug("Downloading processed data (as single file). Project: %s", project())
      
      downloadProcessedDataSingleFile(project(), file)
    }
  )
}

#######################
# HELPERS
#######################

loadProjectInfo <- function(projectName, basePath = BASE_PATH){
  path <- file.path(basePath, projectName, "projectInfo.json")
  flog.debug("loading project info: %s", path)
  rlist::list.load(path)
}

downloadProcessedDataAsZip <- function(project, outputFile, basePath = BASE_PATH){
  
  # temp dir to store processed data for zipping
  temp <- "tempDirForZipping"
  unlink(temp, recursive = TRUE)
  dir.create(temp)
  on.exit(unlink(temp, recursive = TRUE))
  
  projDataPath <- file.path(basePath, project, "data")
  datasetNames <- list.dirs(projDataPath, recursive = FALSE, full.names = FALSE)
  
  walk(datasetNames, ~ {
    processedFile <- file.path(projDataPath, ., "processed.csv")
    if(file.exists(processedFile)){
      path <- file.path(temp, if(endsWith(., ".csv")) . else str_c(., ".csv"))
      file.copy(processedFile, path)
    }
  })
  
  filesToZip <- list.files(temp, full.names = TRUE)
  zip(outputFile, filesToZip, flags = "-r9Xj")
}

downloadProcessedDataSingleFile <- function(project, outputFile, basePath = BASE_PATH){
  
  projDataPath <- file.path(basePath, project, "data")
  processedDatasets <- list.files(
    projDataPath, pattern = "*processed.csv", full.names = TRUE, recursive = TRUE)
  datasetNames <- map_chr(processedDatasets, ~ str_extract(., "(?<=/)[^/]+(?=/processed.csv)"))
  
  allProcessedData <- map(processedDatasets, read_csv) %>%
    map2(., datasetNames, ~ {.x$dataset <- .y; .x}) %>%
    do.call(rbind, .) %>%
    data.frame()
  
  write_csv(allProcessedData, outputFile)
}
