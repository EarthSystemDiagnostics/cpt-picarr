getPathToRawData <- function(name, project, basePath = BASE_PATH, fullPath = TRUE){
  
  pathToDataFolder <- file.path(basePath, project, "data", name)
  
  allCsvFilesInFolder <- list.files(pathToDataFolder, pattern = "*.csv")
  fileName <- allCsvFilesInFolder[!allCsvFilesInFolder %in% c(
    "processingOptions.csv", "sampleDescription.csv", "processed.csv")]
  
  if (fullPath) {
    return(file.path(basePath, project, "data", name, fileName))
  } else {
    return(fileName)
  }
}