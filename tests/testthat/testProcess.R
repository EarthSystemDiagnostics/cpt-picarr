library(testthat)
library(rhandsontable)
library(readr)

context("test output shape for function processDataWithPiccr (calls piccr)")

processingTemplate <- tribble(
  ~`Identifier 1`, ~`Use for drift correction`, ~`Use for calibration`, ~`Use as control standard`, ~`True delta O18`, ~`True delta H2`,
  # ------------ / -------------------------- / --------------------- / ------------------------- / ---------------- / ----------------
  "KARA",              FALSE,                         FALSE,                    FALSE,                  -0.1,                 2,
  "DML",               TRUE,                          TRUE,                     FALSE,                  -42.5,                 2,
  "TD1",               TRUE,                          TRUE,                     FALSE,                  -33.9,                 2,
  "JASE",              TRUE,                          TRUE,                     FALSE,                  -50.22,                 2,
  "NGT",               FALSE,                         FALSE,                    FALSE,                  -34.4,                 2
)
dfA <- read_csv("test_data/HIDS2041_IsoWater_20151126_115726_with_suffix.csv")
dfB <- read_csv("test_data/HIDS2041_IsoWater_20151125_111138_with_suffix.csv")
datasets <- list(
  fileA = dfA,
  fileB = dfB
)
processingOptions <- list(
  fileA = processingTemplate,
  fileB = processingTemplate
)

test_that("overage over all inj", {

  processedData <- processDataWithPiccr(
    datasets, processingOptions, useMemoryCorrection = T, calibrationFlag = 1,
    useThreePointCalibration = T, averageOverLastNInj = "all")

  expect_length(processedData, 2)
  
  actualA <- processedData[[1]]
  expect_equal(actualA$name, "fileA")
  expect_equal(actualA$raw, dfA)
  
  actualB <- processedData[[2]]
  expect_equal(actualB$name, "fileB")
  expect_equal(actualB$raw, dfB)
  
  for (dataset in processedData){
    expect_is(dataset$memoryCorrected, "data.frame")
    expect_is(dataset$calibrated, "data.frame")
    expect_is(dataset$calibratedAndDriftCorrected, "data.frame")
    expect_is(dataset$processed, "data.frame")
    expect_is(dataset$deviationsFromTrue, "data.frame")
    expect_is(dataset$deviationOfControlStandard, "list")
    expect_is(dataset$rmsdDeviationsFromTrue, "list")
    expect_is(dataset$pooledSD, "list")
    expect_is(dataset$memoryCoefficients, "data.frame")
  }
})

test_that("overage over 2 inj", {

  processedData <- processDataWithPiccr(
    datasets, processingOptions, useMemoryCorrection = T, calibrationFlag = 1,
    useThreePointCalibration = T, averageOverLastNInj = 2)

  expect_length(processedData, 2)
  
  actualA <- processedData[[1]]
  expect_equal(actualA$name, "fileA")
  expect_equal(actualA$raw, dfA)
  
  actualB <- processedData[[2]]
  expect_equal(actualB$name, "fileB")
  expect_equal(actualB$raw, dfB)
  
  for (dataset in processedData){
    expect_is(dataset$memoryCorrected, "data.frame")
    expect_is(dataset$calibrated, "data.frame")
    expect_is(dataset$calibratedAndDriftCorrected, "data.frame")
    expect_is(dataset$processed, "data.frame")
    expect_is(dataset$deviationsFromTrue, "data.frame")
    expect_is(dataset$deviationOfControlStandard, "list")
    expect_is(dataset$rmsdDeviationsFromTrue, "list")
    expect_is(dataset$pooledSD, "list")
    expect_is(dataset$memoryCoefficients, "data.frame")
  }
})

test_that("number of inj to average over matters", {

  processedDataTwoInj <- processDataWithPiccr(
    datasets, processingOptions, useMemoryCorrection = T, calibrationFlag = 1,
    useThreePointCalibration = T, averageOverLastNInj = 2)

  processedDataAllInj <- processDataWithPiccr(
    datasets, processingOptions, useMemoryCorrection = T, calibrationFlag = 1,
    useThreePointCalibration = T, averageOverLastNInj = "all")

  expect_failure(expect_equal(processedDataAllInj[[1]]$processed, processedDataTwoInj[[1]]$processed))
  expect_failure(expect_equal(processedDataAllInj[[2]]$processed, processedDataTwoInj[[2]]$processed))
})
