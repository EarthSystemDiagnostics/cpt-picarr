library(testthat)
library(tidyverse)

context("test the function getDatasetsInDateRange")

test_that("test normal case", {
  
  # ------- INITIALIZE INPUTS --------
  
  basePath <- file.path(tempdir(), "testGetDatasetsInDateRange")
  dir.create(basePath)
  on.exit(unlink(basePath, recursive = TRUE))
  
  project <- "Project A"
  dataPath <- file.path(basePath, project, "data")
  dir.create(dataPath, recursive = TRUE)
  
  dataset1Path <- file.path(dataPath, "dataset 1")
  dir.create(dataset1Path)
  write_csv(tibble(col = c(1,2)), file.path(dataset1Path, "raw.csv"))
  list.save(list(additionalInfo = "some info", date = "2015-11-25", device = "device A"), 
            file.path(dataset1Path, "fileInfo.json"))
  file.create(file.path(dataset1Path, "processed.csv"))
  file.create(file.path(dataset1Path, "processingOptions.csv"))
  file.create(file.path(dataset1Path, "sampleDescription.csv"))
  
  
  dataset2Path <- file.path(dataPath, "dataset 2")
  dir.create(dataset2Path)
  write_csv(tibble(col = c("a", "b")), file.path(dataset2Path, "bcdef45678.csv"))
  list.save(
    list(additionalInfo = "additional info yay", date = "1990-07-03", device = "device B"), 
    file.path(dataset2Path, "fileInfo.json"))
  
  dataset3Path <- file.path(dataPath, "dataset 3")
  dir.create(dataset3Path)
  write_csv(tibble(col = c(1,2,3)), file.path(dataset3Path, "raw.csv"))
  list.save(
    list(additionalInfo = "", date = "2019-01-31", device = ""),
    file.path(dataset3Path, "fileInfo.json"))
  
  dateRange <- c("2010-01-01", "2020-05-30")
  # ------- CALL FUNCTION UNDER TEST --------
  
  output <- getDatasetsInDateRange(dateRange, project, basePath)
  
  # -------- EVALUATE OUTPUT ---------
  
  expect_equal(output, c("dataset 1", "dataset 3"))
})

test_that("test case no data in date range", {
  
  # ------- INITIALIZE INPUTS --------
  
  basePath <- file.path(tempdir(), "testGetDatasetsInDateRange2")
  dir.create(basePath)
  on.exit(unlink(basePath, recursive = TRUE))
  
  dateRange <- c("2010-01-01", "2020-05-30")
  
  # ------- CALL FUNCTION UNDER TEST --------
  
  output <- getDatasetsInDateRange(dateRange, "project", basePath)
  
  # -------- EVALUATE OUTPUT ---------
  
  expect_equal(output, character(0))
  
})