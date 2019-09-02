library(purrr)
library(rlist)

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