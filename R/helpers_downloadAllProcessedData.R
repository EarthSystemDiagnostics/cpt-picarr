library(tidyverse)

downloadProcessedDataAsZip <- function(project, outputFile, basePath){
  
  # temp dir to store processed data for zipping
  temp <- "tempDirForZipping"
  unlink(temp, recursive = TRUE)
  dir.create(temp)
  on.exit(unlink(temp, recursive = TRUE))
  
  projDataPath <- file.path(basePath, project, "data")
  datasetNames <- list.dirs(projDataPath, recursive = FALSE, full.names = FALSE)
  
  walk(datasetNames, ~ {
    processedFile <- file.path(projDataPath, ., "processed.csv")
    if(file.exists(processedFile))
      path <- file.path(temp, if(endsWith(., ".csv")) . else str_c(., ".csv"))
      file.copy(processedFile, path)
  })
  
  filesToZip <- list.files(temp, full.names = TRUE)
  zip(outputFile, filesToZip, flags = "-r9Xj")
}