library(testthat)
library(rhandsontable)
library(readr)

context("test the function process. (calls piccr)")

test_that("test output shape for function process", {
  
  processingTemplate <- tribble(
    ~`Identifier 1`, ~`Use for drift correction`, ~`Use for calibration`, ~`Use as control standard`, ~`True delta O18`, ~`True delta H2`,
    # ------------ / -------------------------- / --------------------- / ------------------------- / ---------------- / ----------------
    "KARA",              FALSE,                         FALSE,                    FALSE,                  -0.1,                 2,
    "DML",               TRUE,                          TRUE,                     FALSE,                  -42.5,                 2,
    "TD1",               TRUE,                          TRUE,                     FALSE,                  -33.9,                 2,
    "JASE",              TRUE,                          TRUE,                     FALSE,                  -50.22,                 2,
    "NGT",               FALSE,                         FALSE,                    FALSE,                  -34.4,                 2
  )
  datasets <- list(
    df1 = read_csv("test_data/HIDS2041_IsoWater_20151126_115726.csv"), 
    df2 = read_csv("test_data/HIDS2041_IsoWater_20151125_111138.csv"), 
    df3 = read_csv("test_data/HIDS2041_IsoWater_20151127_143940.csv")
  )
  
  input <- list(
    useMemoryCorrection = TRUE,
    driftAndCalibration = "1/T"
  )
  
  
  processedData <- process(input, processingTemplate, datasets)
  
  expect_named(processedData)
  expect_length(processedData, 4)
  expect_length(processedData$processed, 3)
  expect_length(processedData$memoryCorrected, 3)
  expect_length(processedData$pooledStdDev, 3)
  expect_length(processedData$calibrated, 3)
})