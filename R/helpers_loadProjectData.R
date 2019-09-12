library(tidyverse)

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
  flog.debug("loading data info: %s", projectDataPath)
  
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
    date <- lubridate::ymd_hms(rawData$`Time Code`) %>%
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