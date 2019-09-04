library(testthat)

test_that("test getNamesOfDatasetsInProject", {
  
  basePath <- file.path(tempdir(), "testGetDatasetNamesForProject")
  
  dir.create(file.path(basePath, "Project A", "data"), recursive = TRUE)
  dir.create(file.path(basePath, "Project A", "data", "datasetA"))
  dir.create(file.path(basePath, "Project A", "data", "my data A"))
  dir.create(file.path(basePath, "Project A", "data", "123.456"))
  
  dir.create(file.path(basePath, "Project B", "data", "fileB"), recursive = TRUE)
  
  dir.create(file.path(basePath, "Project C"))
  
  expect_equal(
    getNamesOfDatasetsInProject("Project A", basePath),
    c("123.456", "datasetA", "my data A")
  )
  expect_equal(
    getNamesOfDatasetsInProject("Project B", basePath),
    c("fileB")
  )
  expect_equal(
    getNamesOfDatasetsInProject("Project C", basePath),
    character(0)
  )
})