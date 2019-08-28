library(testthat)
library(readr)
library(tibble)

context("test function saveNewTemplate")

test_that("test save sample description template", {
  
  basePath = file.path(tempdir(), "outputTestSaveNewTemplate")
  dir.create(file.path(basePath, "templates", "sample_description"), recursive = TRUE)
  on.exit(unlink(basePath, recursive = TRUE))
  
  data <- tribble(
    ~colA, ~colB,
    1,     2
  )
  name <- "testTemplate"
  
  saveNewTemplateSampleDescr(data, name, basePath)
  
  filePath <- file.path(basePath, "templates", "sample_description", name)
  expect_true(file.exists(filePath))
  expect_equal(data, read_csv(filePath))
})

test_that("test save processing template", {
  
  basePath = file.path(tempdir(), "outputTestSaveNewTemplate")
  dir.create(file.path(basePath, "templates", "processing"), recursive = TRUE)
  on.exit(unlink(basePath, recursive = TRUE))
  
  data <- tribble(
    ~colA, ~colB,
    1,     2
  )
  name <- "testTemplate"
  
  saveNewTemplateProcessing(data, name, basePath)
  
  filePath <- file.path(basePath, "templates", "processing", name)
  expect_true(file.exists(filePath))
  expect_equal(data, read_csv(filePath))
})