setupDirectoryStructure <- function(basePath){
  
  dir.create(basePath, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(basePath, "processingOptions"), showWarnings = FALSE)
}