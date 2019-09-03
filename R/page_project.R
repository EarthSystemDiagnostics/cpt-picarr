library(shiny)
library(tidyverse)
library(futile.logger)

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
      # TODO
      h3("Project Data"),
      p("TODO")
    ),
    wellPanel(
      h3("What do you want to do next?"),
      
      actionButton(ns("goToGenerateSampleDescr"), "Generate a sample description", style = blue),
      actionButton(ns("goToUploadData"), "Upload measurement data", style = blue),
      actionButton(ns("goToProcessData"), "Process measurement data", style = blue)
      
    )
  )
}


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
  
  # ------------ REACT TO USER INPUT --------------
  
  # render output on the project page
  observe({
    
    projectName <- project()
    req(projectName)
    
    # display project information
    projectInfo           <- loadProjectInfo(projectName)
    output$projName       <- renderText(projectInfo$name)
    output$projPeople     <- renderText(projectInfo$people)
    output$projAdditional <- renderText(projectInfo$additionalInfo)
    output$projDate       <- renderText(projectInfo$date)
    
    # TODO: display project data
    
  })
}

####################
# HELPERS
####################

loadProjectInfo <- function(projectName, basePath = BASE_PATH){
  path <- file.path(basePath, projectName, "projectInfo.json")
  flog.debug(sprintf("loading project info: %s", path))
  rlist::list.load(path)
}