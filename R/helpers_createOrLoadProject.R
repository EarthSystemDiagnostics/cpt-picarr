library(purrr)
library(rlist)

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