library(shiny)

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

pageHome <- function(input, output, session, serverEnvironment){
  
  updateProjectSelection(session)
  
  observeEvent(input$loadProject, {
    setProject(input$projectToLoad, serverEnvironment)
    goToTab("Project", session, serverEnvironment)
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
      goToTab("Project", session, serverEnvironment)
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

setProject <- function(newProject, envir){
  
  # set the reactive value rv$project to the selected project. (Used for
  # communicating the selected project to other modules)
  assign(".newProject", newProject, envir = envir)
  evalq(rv$project <- .newProject, envir = envir)
}