library(testthat)
library(readr)
library(tibble)

context("test function saveNewTemplate")

test_that("test save sample description template", {
  
  basePath <- file.path(tempdir(), "outputTestSaveNewTemplate")
  project <- "Project A"
  dir.create(file.path(basePath, project, "templates", "sampleDescription"), recursive = TRUE)
  on.exit(unlink(basePath, recursive = TRUE))
  
  data <- tribble(
    ~colA, ~colB,
    1,     2
  )
  name <- "testTemplate"
  
  saveNewTemplate(data, name, project, "sampleDescription", basePath)
  
  filePath <- file.path(basePath, project, "templates", "sampleDescription", name)
  expect_true(file.exists(filePath))
  expect_equal(data, read_csv(filePath))
})

test_that("test save sample description template (template folder does not exist)", {
  
  basePath <- file.path(tempdir(), "outputTestSaveNewTemplate")
  project <- "Project A"
  dir.create(file.path(basePath, project), recursive = TRUE)
  on.exit(unlink(basePath, recursive = TRUE))
  
  data <- tribble(
    ~colA, ~colB,
    1,     2
  )
  name <- "testTemplate"
  
  saveNewTemplate(data, name, project, "sampleDescription", basePath)
  
  filePath <- file.path(basePath, project, "templates", "sampleDescription", name)
  expect_true(file.exists(filePath))
  expect_equal(data, read_csv(filePath))
})

test_that("test save processing template", {
  
  basePath <- file.path(tempdir(), "outputTestSaveNewTemplate")
  project <- "Project B"
  dir.create(file.path(basePath, project, "templates", "processing"), recursive = TRUE)
  on.exit(unlink(basePath, recursive = TRUE))
  
  data <- tribble(
    ~colA, ~colB,
    1,     2
  )
  name <- "testTemplate"
  
  saveNewTemplate(data, name, project, "processing", basePath)
  
  filePath <- file.path(basePath, project, "templates", "processing", name)
  expect_true(file.exists(filePath))
  expect_equal(data, read_csv(filePath))
})

test_that("test save processing template (template folder does not exist)", {
  
  basePath <- file.path(tempdir(), "outputTestSaveNewTemplate")
  project <- "Project B"
  dir.create(file.path(basePath, project), recursive = TRUE)
  on.exit(unlink(basePath, recursive = TRUE))
  
  data <- tribble(
    ~colA, ~colB,
    1,     2
  )
  name <- "testTemplate"
  
  saveNewTemplate(data, name, project, "processing", basePath)
  
  filePath <- file.path(basePath, project, "templates", "processing", name)
  expect_true(file.exists(filePath))
  expect_equal(data, read_csv(filePath))
})