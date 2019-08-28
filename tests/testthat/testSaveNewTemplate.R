library(testthat)
library(readr)
library(tibble)

context("test function saveNewTemplate")

test_that("test", {
  
  basePath = file.path(tempdir(), "outputTestSaveNewTemplate")
  dir.create(file.path(basePath, "templates"), recursive = TRUE)
  on.exit(unlink(basePath, recursive = TRUE))
  
  data <- tribble(
    ~colA, ~colB,
    1,     2
  )
  name <- "testTemplate"
  
  saveNewTemplate(data, name, basePath)
  
  filePath <- file.path(basePath, "templates", name)
  
  expect_true(file.exists(filePath))
  expect_equal(data, read_csv(filePath))
})