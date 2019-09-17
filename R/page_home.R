library(shiny)


#' pageHomeUI
#' 
#' UI function for the page 'Home'
#'
#' @param id Identifier for the namespace of this module
#'
#' @return A HTML tag object
pageHomeUI <- function(id){
  
  # create namespace function
  ns <- NS(id)
  
  tagList(
    h2("Welcome to Cpt. Picarr!"),
    h4("What do you want to do today?"),
    
    fluidRow(
      column(4,
        wellPanel(
          h3("Load an existing project"),
          p("See information and data for an existing project. Next you can generate 
                  a sample description, upload measurement data, or process measurement data."),
          selectInput(ns("projectToLoad"), "Choose a project", c()),
          actionButton(ns("loadProject"), "Load selected project", style = blue)
        )
      ),
      column(4,
        wellPanel(
          h3("Create a new project"),
          p("Create a new projecte to manage related data."),
          textInput(ns("projectName"), "Project name"),
          textInput(ns("projectPeople"), "People involved (optional)"),
          textAreaInput(ns("projectAdditionalInfo"), "Additional info (optional)"),
          dateInput(ns("projectDate"), "Expedition date (optional)", value = NA),
          actionButton(ns("createProject"), "Create new project", style = blue),
          textOutput(ns("infoMessage"))
        )
      ),
      column(4,
        wellPanel(
          h3("Instrument performance"),
          p("Look at cross-project statistics to analyze the performance 
            of your measurement instruments over time."),
          actionButton(ns("goToInstrumentPerformance"), "Go to page 'Instrument performance'", style = blue)
        ),
        wellPanel(
          h3("Manage devices"),
          p("List known isotope measurement devices or add a new device."),
          actionButton(ns("goToManageDevices"), "Manage devices", style = blue)
        )
      )
    )
  )
}

#' pageHome
#'
#' Implements the server logic for the page 'Home'.
#' 
#' Note that this module modifies the environment that calls the 
#' module (aka the serverEnvironment). Specifically the value 
#' 'rv$project' is updated in the serverEnvironment when a project is
#' loaded or a new project is created. 
#' This means that a function that calls this module needs to define
#' the variable rv as 'rv <- reactiveValues()'. Also the calling function
#' should initialize rv$project: 'rv$project <- NULL'.
#' The reason why this module modifies the calling function's environment
#' is that the name of the currently loaded project is shared state
#' between the different submodules (all pages need to know which project
#' is loaded). Modifying a reactive value in the most high-level server 
#' function makes it possible to propagate the state change to all other
#' modules as a reactive expression.
#'
#' @param input Shiny inputs
#' @param output Shiny outputs
#' @param session Shiny session
#' @param serverEnvironment An environment. The environment of the 
#'                          server function that calls this module.
#'                          Used to execute code in the environment of the 
#'                          main server function for the app (e.g. to 
#'                          switch between pages).
#'
#' @return No explicit return value
pageHome <- function(input, output, session, serverEnvironment){
  
  updateProjectSelection(session)
  
  observeEvent(input$loadProject, {
    setProject(input$projectToLoad, serverEnvironment)
    goToPage("Project", serverEnvironment)
  })
  
  observeEvent(input$createProject, {
    
    name <- input$projectName
    req(name)
    
    if (projectExistsAlready(name)) {
      output$infoMessage <- renderText(
        sprintf("Project named '%s' exists already. Please choose a different name.", name))
    } else {
      createProjectDirectory(name)
      createProjectInfoFile(input)
      updateProjectSelection(session)
      
      output$infoMessage <- renderText("New project created.")
      
      setProject(name, serverEnvironment)
      goToPage("Project", serverEnvironment)
    }
  })
  
  observeEvent(input$goToInstrumentPerformance, {
    goToPage("Instrument performance", serverEnvironment)
  })
  
  observeEvent(input$goToManageDevices, {
    goToPage("Manage devices", serverEnvironment)
  })
}

#######################
# HELPERS
#######################

updateProjectSelection <- function(session){
  # add existing projects to selection dropdown for "Load an existing project"
  updateSelectInput(session, "projectToLoad", choices = getExistingProjects())
}

getExistingProjects <- function(basePath = BASE_PATH){
  dirs <- list.dirs(basePath, recursive = FALSE, full.names = FALSE)
  
  # a project is a folder that contains a projectInfo.json file
  projects <- purrr::keep(dirs, ~ file.exists(file.path(basePath, ., "projectInfo.json")))
  return(projects)
}

setProject <- function(newProject, envir){
  
  # set the reactive value rv$project to the selected project. (Used for
  # communicating the selected project to other modules)
  assign(".newProject", newProject, envir = envir)
  evalq(rv$project <- .newProject, envir = envir)
}