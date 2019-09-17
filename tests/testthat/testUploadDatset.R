library(testthat)
library(readr)
library(rlist)

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

test_that("test no device given", {
  
  input <- list(
    file = "some file",
    name = "some name",
    device = NULL
  )
  
  messageActual <- uploadDataset(input, "project")
  
  expect_equal(messageActual, "You need to select a device before clicking this button.")
})

test_that("upload data and fetching processing options", {
  
  basePath <- file.path(tempdir(), "testUploadDataset")
  dir.create(file.path(basePath, "Project A", "data"), recursive = TRUE)
  on.exit(unlink(basePath, recursive = TRUE))
  
  # create test dataset
  data <- tibble(
    `Identifier 2` = c("_123.456", "a_123.456"), 
    `Time Code` = c("2019/08/31 16:06:46", "2019/08/31 16:06:46"), 
    colA = c(1.2, 4.6)
  )
  filePath <- file.path(tempdir(), "testData.csv")
  write_csv(data, filePath)
  
  # create the sample description and processing options
  processingOptions <- tibble(colA = c("a", "b"), colB = c(3.5, 7.2))
  sampleDescription <- tibble(colB = c(4.5, 6.7), colC = c(1.2, 4.5))
  optionsPath <- file.path(basePath, "processingOptions", "123.456")
  dir.create(optionsPath, recursive = TRUE)
  write_csv(processingOptions, file.path(optionsPath, "processingOptions.csv"))
  write_csv(sampleDescription, file.path(optionsPath, "sampleDescription.csv"))
  
  input <- list(
    file = tibble(name = c("devicename_filename"), datapath = c(filePath)),
    name = "dataset A",
    info = "some info",
    device = "device A (code A)"
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
  expect_equal(
    messageActual, 
    sprintf("Dataset sucessfully uploaded. Processing Options and 
                 sample descriptions were found. (The data is in %s)", outputDir)
  )
  expect_equal(
    read_csv(file.path(outputDir, "devicename_filename")),
    tibble(`Identifier 2` = c(NA, "a"), colA = c(1.2, 4.6), 
           `Time Code` = c("2019/08/31 16:06:46", "2019/08/31 16:06:46"))
  )
  expect_true(file.exists(file.path(outputDir, "fileInfo.json")))
  expect_equal(
    list.load(file.path(outputDir, "fileInfo.json")),
    list(date = "2019-08-31", device = "device A (code A)", additionalInfo = "some info")
  )
})

test_that("no processing options saved for uploaded dataset", {
  
  basePath <- file.path(tempdir(), "testUploadDataset")
  dir.create(file.path(basePath, "Project A", "data"), recursive = TRUE)
  on.exit(unlink(basePath, recursive = TRUE))
  
  # create test dataset
  data <- tibble(`Identifier 2` = c("_abc.def", "a_abc.def"), colA = c(1.2, 4.6))
  filePath <- file.path(tempdir(), "testData.csv")
  write_csv(data, filePath)
  
  input <- list(
    file = tibble(name = c("file name"), datapath = c(filePath)),
    name = "dataset A",
    info = "some info",
    device = "some device"
  )
  
  messageExpected <- "Error: Could not find processing options for the uploaded dataset. (unique id: abc.def)"
  
  messageActual <- uploadDataset(input, "Project A", basePath)
  
  expect_equal(
    messageActual, 
    messageExpected)
})

test_that("no unique identifier in uploaded dataset", {
  
  basePath <- file.path(tempdir(), "testUploadDataset")
  dir.create(file.path(basePath, "Project A", "data"), recursive = TRUE)
  on.exit(unlink(basePath, recursive = TRUE))
  
  # create test dataset
  data <- tibble(`Identifier 2` = c("x", ""), colA = c(1.2, 4.6))
  filePath <- file.path(tempdir(), "testData.csv")
  write_csv(data, filePath)
  
  input <- list(
    file = tibble(name = c("file name"), datapath = c(filePath)),
    name = "dataset A",
    info = "some info",
    device = "abc"
  )
  
  messageExpected <- "Error: No unique identifier found in the uploaded dataset."
  
  messageActual <- uploadDataset(input, "Project A", basePath)
  
  expect_equal(
    messageActual, 
    messageExpected)
})

test_that("dataset with same name exists already", {
  
  basePath <- file.path(tempdir(), "testUploadDataset")
  dir.create(file.path(basePath, "Project A", "data"), recursive = TRUE)
  on.exit(unlink(basePath, recursive = TRUE))
  
  # create the folder that causes the naming conflict
  outputDir <- file.path(basePath, "Project A", "data", "dataset A")
  dir.create(outputDir)
  
  # create test dataset
  data <- tibble(`Identifier 2` = c("_123.456", "a_123.456"), colA = c(1.2, 4.6))
  filePath <- file.path(tempdir(), "testData.csv")
  write_csv(data, filePath)
  
  # create the sample description and processing options
  processingOptions <- tibble(colA = c("a", "b"), colB = c(3.5, 7.2))
  sampleDescription <- tibble(colB = c(4.5, 6.7), colC = c(1.2, 4.5))
  optionsPath <- file.path(basePath, "processingOptions", "123.456")
  dir.create(optionsPath, recursive = TRUE)
  write_csv(processingOptions, file.path(optionsPath, "processingOptions.csv"))
  write_csv(sampleDescription, file.path(optionsPath, "sampleDescription.csv"))
  
  input <- list(
    file = tibble(name = c("file name"), datapath = c(filePath)),
    name = "dataset A",
    info = "some info",
    device = "abc"
  )
  
  messageActual <- uploadDataset(input, "Project A", basePath)
  
  expect_equal(
    messageActual,
    sprintf("Upload aborted. A dataset with the same name exists already. (path: %s)", outputDir)
  )
})