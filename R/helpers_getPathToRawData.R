getPathToRawData <- function(name, project, basePath = BASE_PATH){
  
  pathToDataFolder <- file.path(basePath, project, "data", name)
  
  allCsvFilesInFolder <- list.files(pathToDataFolder, pattern = "*.csv")
  fileName <- allCsvFilesInFolder[!allCsvFilesInFolder %in% c(
    "processingOptions.csv", "sampleDescription.csv", "processed.csv")]
  
  rawDataPath <- file.path(basePath, project, "data", name, fileName)
  
  return(rawDataPath)
}