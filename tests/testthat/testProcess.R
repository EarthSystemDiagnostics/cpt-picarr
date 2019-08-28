library(testthat)
library(rhandsontable)

context("test the function process. (calls piccr)")

test_that("test output shape for function process", {
  
  basePath <- file.path(tempdir(), "outputTestProcess")
  dir.create(basePath)
  dir.create(file.path(basePath, "data", "123.456"), recursive = TRUE)
  dir.create(file.path(basePath, "data", "abc.def"), recursive = TRUE)
  on.exit(unlink(basePath, recursive = TRUE))
  
  processingTemplate <- tribble(
    ~`Identifier 1`, ~`Use for drift correction`, ~`Use for calibration`, ~`Use as control standard`, ~`True delta O18`, ~`True delta H2`,
    # ------------ / -------------------------- / --------------------- / ------------------------- / ---------------- / ----------------
    "KARA",              FALSE,                         FALSE,                    FALSE,                  -0.1,                 2,
    "DML",               TRUE,                          TRUE,                     FALSE,                  -42.5,                 2,
    "TD1",               TRUE,                          TRUE,                     FALSE,                  -33.9,                 2,
    "JASE",              TRUE,                          TRUE,                     FALSE,                  -50.22,                 2,
    "NGT",               FALSE,                         FALSE,                    FALSE,                  -34.4,                 2
  )
  write_csv(processingTemplate, file.path(basePath, "data", "123.456", "processingOptions.csv"))
  write_csv(processingTemplate, file.path(basePath, "data", "abc.def", "processingOptions.csv"))
  
  files <- list(
    name = c("fileA", "fileB"),
    datapath = c("test_data/HIDS2041_IsoWater_20151126_115726_with_suffix.csv", 
                 "test_data/HIDS2041_IsoWater_20151125_111138_with_suffix.csv")
  )
  input <- list(
    files = files,
    useMemoryCorrection = TRUE,
    driftAndCalibration = "1/T"
  )
  
  processedData <- process(input, basePath)
  
  expect_length(processedData, 2)
  expect_equal(names(processedData), c("fileA", "fileB"))
  
  expect_length(processedData$fileA, 4)
  expect_length(processedData$fileA$memoryCorrected, 1)
  expect_length(processedData$fileA$processed, 1)
  expect_length(processedData$fileA$calibrated, 1)
  expect_length(processedData$fileA$pooledStdDev, 1)
  expect_is(processedData$fileA$memoryCorrected$data$datasetMemoryCorrected, "data.frame")
  expect_is(processedData$fileA$memoryCorrected$data$memoryCoefficients, "data.frame")
  expect_is(processedData$fileA$processed$data, "data.frame")
  expect_is(processedData$fileA$calibrated$data, "data.frame")
})