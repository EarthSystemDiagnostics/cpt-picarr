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
  
  map(names(datasets), processSingleDataset, processingOptions = processingOptions,
       datasets = datasets,  config = config)
}

processSingleDataset <- function(datasetName, processingOptions, datasets, config){
  
  config$standards <- getOptionsAndTrueValuesForStandards(processingOptions[[datasetName]])
  
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
