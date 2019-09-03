library(testthat)
library(readr)

context("test uploading a dataset")

test_that("test no file uploaded", {
  
  input <- list(
    file = NULL
  )
  
  messageActual <- uploadDataset(input, "project")
  
  expect_equal(messageActual, "You need to upload a file before clicking this button.")
})

test_that("test no name given", {
  
  input <- list(
    file = "some file",
    name = NULL
  )
  
  messageActual <- uploadDataset(input, "project")
  
  expect_equal(messageActual, "You need to name the dataset before clicking this button.")
})

test_that("upload data and fetching processing options", {
  
  basePath <- file.path(tempdir(), "testUploadDataset")
  dir.create(file.path(basePath, "Project A", "data"), recursive = TRUE)
  on.exit(unlink(basePath, recursive = TRUE))
  
  # create test dataset
  data <- tibble(`Identifier 2` = c("_123.456", "a_123.456"), colA = c(1.2, 4.6))
  filePath <- file.path(tempdir(), "testData.csv")
  write_csv(data, filePath)
  
  # create the sample description and processing options
  processingOptions <- tibble(colA = c("a", "b"))
  sampleDescription <- tibble(colB = c(4.5, 6.7))
  optionsPath <- file.path(basePath, "processingOptions", "123.456")
  dir.create(optionsPath, recursive = TRUE)
  write.csv(processingOptions, file.path(optionsPath, "processingOptions.csv"))
  write.csv(sampleDescription, file.path(optionsPath, "sampleDescription.csv"))
  
  input <- list(
    file = tibble(name = c("file name"), datapath = c(filePath)),
    name = "dataset A",
    info = "some info"
  )
  
  messageActual <- uploadDataset(input, "Project A", basePath)
  
  outputDir <- file.path(basePath, "Project A", "data", "dataset A")
  expect_true(file.exists(file.path(outputDir, "processingOptions.csv")))
  expect_true(file.exists(file.path(outputDir, "sampleDescription.csv")))
  expect_equal(
    read_csv(file.path(outputDir, "sampleDescription.csv")),
    sampleDescription
  )
  expect_equal(
    read_csv(file.path(outputDir, "processingOptions.csv")),
    processingOptions
  )
  
  expect_equal(messageActual, "Dataset sucessfully uploaded.")
  expect_equal(
    read_csv(file.path(outputDir, "file name")),
    tibble(`Identifier 2` = c("", "a"), colA = c(1.2, 4.6))
  )
})