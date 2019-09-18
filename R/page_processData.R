library(tidyverse)
library(rhandsontable)
library(piccr)
library(futile.logger)

#' pageProcessDataUI
#' 
#' UI function for the page 'Process measurement data'
#'
#' @param id Identifier for the namespace of this module
#'
#' @return A HTML tag object
pageProcessDataUI <- function(id){
  
  # create namespace function
  ns <- NS(id)
  
  tagList(
    h2("Process isotope measurement data"),
    textOutput(ns("projectName")), br(),
    
    pageProcessDataOptionsUI(ns("options")),
    
    wellPanel(
      h4("Plots and tables"),
      uiOutput(ns("plots"))
    ),
    
    actionButton(ns("goToPageProject"), "Back to page 'Project'")
  )
}

#' pageProcessData
#'
#' Implements the server logic for the page 'Process measurement data'.
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
pageProcessData <- function(input, output, session, id, project, serverEnvironment, projectDataChanged){
  
  # ---------------- CREATE OWN REACTIVE VALUES ---------
  
  rv <- reactiveValues()
  rv$processedData <- NULL
  rv$processingSuccessful <- FALSE
  
  processingSuccessful <- reactive({rv$processingSuccessful})
  processedData <- reactive({rv$processedData})
  
  # create a namespace function
  ns <- NS(id)

  # --------------- DISPLAY PROJECT NAME -------------
  
  output$projectName <- renderText(sprintf("Project: %s", project()))
  
  # --------------- RENDER PLOTS SECTION --------------
  
  output$plots <- renderUI({
    if (!processingSuccessful()){
      p("This section will contain plots and tables once you have processed some data.")
    } else {
      pageProcessDataPlotsUI(ns("plots"))
    }
  })
  
  # ---------------- NAVIGATION ------------------
  
  observeEvent(input$goToPageProject, goToPage("Project", serverEnvironment))
  
  # ---------------- CALL SUBMODULES -------------
  callModule(
    pageProcessDataOptions, ns("options"),
    id = "options",
    projectDataChanged = projectDataChanged, 
    project = project, 
    serverEnvironment = environment()
  )
  callModule(
    pageProcessDataPlots, ns("plots"), 
    id = "plots",
    processingSuccessful = processingSuccessful, 
    processedData = processedData
  )
}
