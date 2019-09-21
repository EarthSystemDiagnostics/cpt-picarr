library(testthat)
library(rhandsontable)
library(readr)

context("test output shape for function processDataWithPiccr (calls piccr)")

# TODO: add test back in when new piccr interface is integrated

# processingTemplate <- tribble(
#   ~`Identifier 1`, ~`Use for drift correction`, ~`Use for calibration`, ~`Use as control standard`, ~`True delta O18`, ~`True delta H2`,
#   # ------------ / -------------------------- / --------------------- / ------------------------- / ---------------- / ----------------
#   "KARA",              FALSE,                         FALSE,                    FALSE,                  -0.1,                 2,
#   "DML",               TRUE,                          TRUE,                     FALSE,                  -42.5,                 2,
#   "TD1",               TRUE,                          TRUE,                     FALSE,                  -33.9,                 2,
#   "JASE",              TRUE,                          TRUE,                     FALSE,                  -50.22,                 2,
#   "NGT",               FALSE,                         FALSE,                    FALSE,                  -34.4,                 2
# )
# 
# datasets <- list(
#   fileA = read_csv("test_data/HIDS2041_IsoWater_20151126_115726_with_suffix.csv"),
#   fileB = read_csv("test_data/HIDS2041_IsoWater_20151125_111138_with_suffix.csv")
# )
# processingOptions <- list(
#   fileA = processingTemplate,
#   fileB = processingTemplate
# )
# 
# test_that("overage over all inj", {
#   
#   processedData <- processDataWithPiccr(
#     datasets, processingOptions, useMemoryCorrection = T, calibrationFlag = 1, 
#     useThreePointCalibration = T, averageOverLastNInj = "all")
#   
#   expect_length(processedData, 2)
#   expect_equal(names(processedData), c("fileA", "fileB"))
#   
#   expect_length(processedData$fileA, 4)
#   expect_length(processedData$fileA$memoryCorrected, 1)
#   expect_length(processedData$fileA$processed, 1)
#   expect_length(processedData$fileA$calibrated, 1)
#   expect_length(processedData$fileA$pooledStdDev, 1)
#   expect_is(processedData$fileA$memoryCorrected$data$datasetMemoryCorrected, "data.frame")
#   expect_is(processedData$fileA$memoryCorrected$data$memoryCoefficients, "data.frame")
#   expect_is(processedData$fileA$processed$data, "data.frame")
#   expect_is(processedData$fileA$calibrated$data, "data.frame")
# })
# 
# test_that("overage over 2 inj", {
#   
#   processedData <- processDataWithPiccr(
#     datasets, processingOptions, useMemoryCorrection = T, calibrationFlag = 1, 
#     useThreePointCalibration = T, averageOverLastNInj = 2)
#   
#   expect_length(processedData, 2)
#   expect_equal(names(processedData), c("fileA", "fileB"))
#   
#   expect_length(processedData$fileA, 4)
#   expect_length(processedData$fileA$memoryCorrected, 1)
#   expect_length(processedData$fileA$processed, 1)
#   expect_length(processedData$fileA$calibrated, 1)
#   expect_length(processedData$fileA$pooledStdDev, 1)
#   expect_is(processedData$fileA$memoryCorrected$data$datasetMemoryCorrected, "data.frame")
#   expect_is(processedData$fileA$memoryCorrected$data$memoryCoefficients, "data.frame")
#   expect_is(processedData$fileA$processed$data, "data.frame")
#   expect_is(processedData$fileA$calibrated$data, "data.frame")
# })
# 
# test_that("number of inj to average over matters", {
#   
#   processedDataTwoInj <- processDataWithPiccr(
#     datasets, processingOptions, useMemoryCorrection = T, calibrationFlag = 1, 
#     useThreePointCalibration = T, averageOverLastNInj = 2)
#   
#   processedDataAllInj <- processDataWithPiccr(
#     datasets, processingOptions, useMemoryCorrection = T, calibrationFlag = 1, 
#     useThreePointCalibration = T, averageOverLastNInj = "all")
#   
#   expect_failure(expect_equal(processedDataAllInj$fileA$processed, processedDataTwoInj$fileA$processed))
#   expect_failure(expect_equal(processedDataAllInj$fileB$processed, processedDataTwoInj$fileB$processed))
# })
