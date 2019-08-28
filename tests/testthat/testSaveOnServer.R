library(testthat)
library(readr)

context("test saving sample description and processing options on the server")

test_that("test", {
  
  basePath <- file.path(tempdir(), "testSaveOnServer")
  dir.create(BASE_PATH)
  on.exit(unlink(BASE_PATH, recursive = TRUE))
  
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
  
  path <- file.path(basePath, "data", uniqueIdentifier)
  expect_true(dir.exists(path))
  expect_equal(sampleDescr, read_csv(file.path(path, "sampleDescription.csv")))
  expect_equal(processingOptions, read_csv(file.path(path, "processingOptions.csv")))
})