library(testthat)
library(tidyverse)

test_that("test loadSelectedDatasets", {
  
  basePath <- file.path(tempdir(), "testLoadSelectedDatasets")
  dir.create(basePath)
  on.exit(unlink(basePath, recursive = TRUE))
  
  # ---------- create directory structure ------------
  
  dir.create(file.path(basePath, "Project A", "data"), recursive = TRUE)
  
  dir.create(file.path(basePath, "Project A", "data", "dataset A"))
  file.create(file.path(basePath, "Project A", "data", "dataset A", "processingOptions.csv"))
  file.create(file.path(basePath, "Project A", "data", "dataset A", "sampleDescription.csv"))
  file.create(file.path(basePath, "Project A", "data", "dataset A", "HIDS_34321_dshl.csv"))
  file.create(file.path(basePath, "Project A", "data", "dataset A", "fileInfo.json"))
  file.create(file.path(basePath, "Project A", "data", "dataset A", "processed.csv"))
  
  dir.create(file.path(basePath, "Project A", "data", "dataset B"))
  file.create(file.path(basePath, "Project A", "data", "dataset B", "3456.5678_rhgjh.csv"))
  file.create(file.path(basePath, "Project A", "data", "dataset B", "processingOptions.csv"))
  file.create(file.path(basePath, "Project A", "data", "dataset B", "sampleDescription.csv"))
  
  dir.create(file.path(basePath, "Project A", "data", "dataset C"))
  file.create(file.path(basePath, "Project A", "data", "dataset C", "x y z.csv"))
  
  dir.create(file.path(basePath, "Project A", "data", "dataset D"))
  file.create(file.path(basePath, "Project A", "data", "dataset D", "x y z.csv"))
  
  dir.create(file.path(basePath, "Project A", "data", "dataset E"))
  file.create(file.path(basePath, "Project A", "data", "dataset E", "abc.csv"))
  
  # ---------- create mock data ---------------
  
  datasetA <- tibble(a = c("a", "b"))
  datasetB <- tibble(b = c("d", "e"))
  datasetC <- tibble(c = c("f", "g"))
  datasetD <- tibble(d = c("h", "i"))
  datasetE <- tibble(e = c("j", "k"))
  
  write_csv(datasetA, file.path(basePath, "Project A", "data", "dataset A", "HIDS_34321_dshl.csv"))
  write_csv(datasetB, file.path(basePath, "Project A", "data", "dataset B", "3456.5678_rhgjh.csv"))
  write_csv(datasetC, file.path(basePath, "Project A", "data", "dataset C", "x y z.csv"))
  write_csv(datasetD, file.path(basePath, "Project A", "data", "dataset D", "x y z.csv"))
  write_csv(datasetE, file.path(basePath, "Project A", "data", "dataset E", "abc.csv"))
  
  # ---------- create vars for input args --------------
  
  selected <- list("dataset A", "dataset B", "dataset C", "dataset D")
  project <- "Project A"
  
  # ---------- call the function under test --------------
  
  actual <- loadSelectedDatasets(selected, project, basePath)
  
  # ---------- test output ------------------
  
  expect_length(actual, 4)
  expect_equal(actual$`dataset A`, datasetA)
  expect_equal(actual$`dataset B`, datasetB)
  expect_equal(actual$`dataset C`, datasetC)
  expect_equal(actual$`dataset D`, datasetD)
})

test_that("test loadSelectedProcessingOptions", {
  
  basePath <- file.path(tempdir(), "testLoadSelectedDatasets")
  dir.create(basePath)
  on.exit(unlink(basePath, recursive = TRUE))
  
  # ---------- create directory structure ------------
  
  dir.create(file.path(basePath, "Project A", "data"), recursive = TRUE)
  
  dir.create(file.path(basePath, "Project A", "data", "dataset A"))
  file.create(file.path(basePath, "Project A", "data", "dataset A", "processingOptions.csv"))
  file.create(file.path(basePath, "Project A", "data", "dataset A", "sampleDescription.csv"))
  file.create(file.path(basePath, "Project A", "data", "dataset A", "HIDS_34321_dshl.csv"))
  file.create(file.path(basePath, "Project A", "data", "dataset A", "fileInfo.json"))
  file.create(file.path(basePath, "Project A", "data", "dataset A", "processed.csv"))
  
  dir.create(file.path(basePath, "Project A", "data", "dataset B"))
  file.create(file.path(basePath, "Project A", "data", "dataset B", "processingOptions.csv"))
  
  dir.create(file.path(basePath, "Project A", "data", "dataset C"))
  
  dir.create(file.path(basePath, "Project A", "data", "dataset D"))
  file.create(file.path(basePath, "Project A", "data", "dataset D", "processingOptions.csv"))
  
  # ---------- create mock data ---------------
  
  datasetA <- tibble(a = c("a", "b"))
  datasetB <- tibble(b = c("d", "e"))
  datasetD <- tibble(d = c("h", "i"))
  
  write_csv(datasetA, file.path(basePath, "Project A", "data", "dataset A", "processingOptions.csv"))
  write_csv(datasetB, file.path(basePath, "Project A", "data", "dataset B", "processingOptions.csv"))
  write_csv(datasetD, file.path(basePath, "Project A", "data", "dataset D", "processingOptions.csv"))
  
  # ---------- create vars for input args --------------
  
  selected <- list("dataset A", "dataset B")
  project <- "Project A"
  
  # ---------- call the function under test --------------
  
  actual <- loadSelectedProcessingOptions(selected, project, basePath)
  
  # ---------- test output ------------------
  
  expect_length(actual, 2)
  expect_equal(actual$`dataset A`, datasetA)
  expect_equal(actual$`dataset B`, datasetB)
})