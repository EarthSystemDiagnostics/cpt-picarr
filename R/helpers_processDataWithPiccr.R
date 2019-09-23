library(tidyverse)
library(futile.logger)
library(piccr)

processDataWithPiccr <- function(datasets, processingOptions, useMemoryCorrection, 
                                 calibrationFlag, useThreePointCalibration, averageOverLastNInj){
  
  flog.info(str_c("Processing datasets"))
  
  flog.debug("Generating config")
  config <- list(
    average_over_last_n_inj = averageOverLastNInj,
    use_memory_correction = useMemoryCorrection,
    calibration_method = calibrationFlag,
    use_three_point_calibration = useThreePointCalibration
  )
  
  flog.debug("Processing the loaded datasets")
  processedData <- processDatasets(datasets, processingOptions, config)
  
  flog.debug("Outputting the processed data")
  return(processedData)
}

processDatasets <- function(datasets, processingOptions, config){
  
  map2(names(datasets), processingOptions, processSingleDataset, 
       datasets = datasets,  config = config)
}

processSingleDataset <- function(datasetName, processingOptions, datasets, config){
  
  config$standards <- getOptionsAndTrueValuesForStandards(processingOptions)
  
  data <- list(datasets[[datasetName]])
  names(data) <- c(datasetName)
  
  processedData <- piccr::processData(data, config)
  unlist(processedData, recursive = FALSE)
}

getOptionsAndTrueValuesForStandards <- function(processingOptions){

  columnsRenamed <- transmute(processingOptions,
                              name = `Identifier 1`, 
                              o18_True = `True delta O18`,
                              H2_True = `True delta H2`, 
                              use_for_drift_correction = `Use for drift correction`, 
                              use_for_calibration = `Use for calibration`,
                              use_as_control_standard = `Use as control standard`)
  processingOptionsAsList <- transpose(columnsRenamed)
  
  return(processingOptionsAsList)
}

#############################################

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