library(testthat)
library(readr)

context("test saving data on the server")

test_that("test saving sample description and processing options on the server", {
  
  basePath <- file.path(tempdir(), "testSaveOnServer")
  dir.create(basePath)
  on.exit(unlink(basePath, recursive = TRUE))
  
  sampleDescr <- tribble(
    ~colA, ~colB,
    1,     2
  )
  processingOptions <- tribble(
    ~colA, ~colB,
    5,     6
  )
  uniqueIdentifier <- "34567898765sgdjcbkx.5678"
  
  saveOnServer(sampleDescr, processingOptions, uniqueIdentifier, basePath)
  
  path <- file.path(basePath, "processingOptions", uniqueIdentifier)
  expect_true(dir.exists(path))
  expect_equal(sampleDescr, read_csv(file.path(path, "sampleDescription.csv")))
  expect_equal(processingOptions, read_csv(file.path(path, "processingOptions.csv")))
})

test_that("test saving processed data on the server", {
  
  processedData <- list(
    list(
      name = "fileA",
      processed = tibble(colA = c("a", "b"))
    ),
    list(
      name = "fileB",
      processed = tibble(colB = c(1.4, 5.6))
    )
  )
  project <- "Project A"
  basePath <- file.path(tempdir(), "testSaveProcessedDataOnServer")
  
  dir.create(file.path(basePath, project, "data", "fileA"), recursive = TRUE)
  on.exit(unlink(basePath, recursive = TRUE))
  
  saveProcessedDataOnServer(processedData, project, basePath)
  
  expect_true(file.exists(file.path(basePath, project, "data", "fileA", "processed.csv")))
  expect_true(file.exists(file.path(basePath, project, "data", "fileB", "processed.csv")))
  expect_equal(
    read_csv(file.path(basePath, project, "data", "fileA", "processed.csv")),
    tibble(colA = c("a", "b"))
  )
  expect_equal(
    read_csv(file.path(basePath, project, "data", "fileB", "processed.csv")),
    tibble(colB = c(1.4, 5.6))
  )
})