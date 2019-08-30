library(tidyverse)
library(futile.logger)
library(piccr)


processDataWithPiccr <- function(input, basePath){
  
  flog.info(str_c("Processing datasets"))
  flog.debug(str_c("basePath: ", basePath))
  
  flog.debug("Read input datasets")
  datasets <- readInputDatasets(input)
  flog.debug(str_c("loaded datasets: ", do.call(paste, as.list(names(datasets)))))
  
  flog.debug("Loading processing options")
  processingOptions <- loadProcessingOptions(datasets, basePath)
  
  flog.debug("Generating config")
  config <- generateConfig(input)
  
  flog.debug("Processing the loaded datasets")
  processedData <- processDatasets(datasets, processingOptions, config)
  
  flog.debug("Outputting the processed data")
  return(processedData)
}

readInputDatasets <- function(input){
  datasets <- map(input$files$datapath, ~ read_csv(.))
  names(datasets) <- input$files$name
  return(datasets)
}

loadProcessingOptions <- function(datasets, basePath){
  # Load processing Options for each dataset from disc. (Using the unique identifier coded
  # into the `Identifier 2` column)
  map(datasets, function(dataset){
    firstIdentifier2 <- first(dataset$`Identifier 2`)
    uniqueIdentifier <- str_extract(firstIdentifier2, "(?<=_).+$")
    
    path <- file.path(basePath, "data", uniqueIdentifier)
    processingOptions <- read_csv(file.path(path, "processingOptions.csv"))
    return(processingOptions)
  })
}

generateConfig <- function(input){
  calibrationMethod <- as.numeric(str_split(input$driftAndCalibration, "/")[[1]][[1]])
  useThreePointCalibration <- as.logical(str_split(input$driftAndCalibration, "/")[[1]][[2]])
  
  config <- list(
    average_over_last_n_inj = 3,
    use_memory_correction = input$useMemoryCorrection,
    calibration_method = calibrationMethod,
    use_three_point_calibration = useThreePointCalibration
  )
  return(config)
}

processDatasets <- function(datasets, processingOptions, config){
  
  map2(datasets, processingOptions, function(dataset, processingOptions, config){
    
    standards <- processingOptions %>%
      transmute(name = `Identifier 1`, o18_True = `True delta O18`, H2_True = `True delta H2`, 
                use_for_drift_correction = `Use for drift correction`, use_for_calibration = `Use for calibration`) %>% 
      transpose() 
    config$standards <- standards
    
    piccr::processData(list(data = dataset), config)
  }, config = config)
}