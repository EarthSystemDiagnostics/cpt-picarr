#' Get a List with relevant information about the project. 
#' 
#' Relies on getStoredData(..).
#' 
#' @return A named list. The names correspond to the names of the datsets 
#'         for the given project. Has the following components:
#'           $path (path to the folder with data for this dataset)
#'           $raw  (name of the file with raw data)
#'           $date (measurement date)
#'           $device (serial number of measurement device)
#'           $processed ('processed.csv' if the dataset has been processed)
#'           $additionalInfo
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
  
  dataPath <- file.path(basePath, project, "data", datasetName)
  infoFilePath <- file.path(dataPath, "fileInfo.json")
  fileInfo <- list.load(infoFilePath)
  
  output <- list(
    path = dataPath,
    raw = getPathToRawData(datasetName, project, basePath, fullPath = FALSE),
    additionalInfo = fileInfo$additionalInfo,
    date = fileInfo$date,
    device = fileInfo$device
  )
  
  if(file.exists(file.path(dataPath, "processed.csv"))) 
    output$processed <- "processed.csv"
  
  return(output)
}