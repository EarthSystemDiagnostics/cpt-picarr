library(tidyverse)
library(rlist)

pageHomeUI <- function(id){
  
  # create namespace function
  ns <- NS(id)
  
  tagList(
    h2("Welcome to Cpt. Picarr!"),
    h4("What do you want to do today?"),
    
    wellPanel(
      h3("Load an existing project"),
      p("See information and data for an existing project. Next you can generate 
        a sample description, upload measurement data, or process measurement data."),
      selectInput(ns("projectToLoad"), "Choose a project", c()),
      actionButton(ns("loadProject"), "Load selected project", style = blue)
    ),
    
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
  )
}

pageHome <- function(input, output, session){
  
  updateProjectSelection(session)
  
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
    }
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

projectExistsAlready <- function(projectName, basePath = BASE_PATH){
  dir.exists(file.path(basePath, projectName))
}

createProjectDirectory <- function(projectName, basePath = BASE_PATH){
  dir.create(file.path(basePath, projectName))
}

createProjectInfoFile <- function(input, basePath = BASE_PATH){
  
  projectName <- input$projectName
  
  projectInfo <- list(
    name = projectName,
    people = input$projectPeople,
    additionalInfo = input$projectAdditionalInfo,
    date = input$projectDate
  )
  
  filePath <- file.path(basePath, projectName, "projectInfo.json")
  rlist::list.save(projectInfo, filePath)
}
