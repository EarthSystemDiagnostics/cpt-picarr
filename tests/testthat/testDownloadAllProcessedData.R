library(testthat)
library(readr)

context("test downloading all processed on page 'Project'")

test_that("test downloading processed data as multiple files", {
  
  # ------- INITIALIZE INPUTS ----------
  
  basePath <- file.path(tempdir(), "testDownloadAllDataMultipleFiles")
  dir.create(basePath, recursive = TRUE)
  on.exit(unlink(basePath, recursive = TRUE))
  
  firstProjDataPath <- file.path(basePath, "first project", "data")
  
  dir.create(file.path(firstProjDataPath, "dataset 1"), recursive = TRUE)
  df1 <- tibble(a = c(1.2, 3.4), b = c("a", "b"))
  write_csv(df1, file.path(firstProjDataPath, "dataset 1", "processed.csv"))
  
  dir.create(file.path(firstProjDataPath, "dataset2.csv"), recursive = TRUE)
  df2 <- tibble(a = c(4.6, 7.8), b = c("x", "y"))
  write_csv(df2, file.path(firstProjDataPath, "dataset2.csv", "processed.csv"))
  
  # create project, whose data should not be downloaded
  secondProjDataPath <- file.path(basePath, "second project", "data")
  dir.create(file.path(secondProjDataPath, "dataset"), recursive = TRUE)
  write_csv(tibble(x = c(1,2,3)), file.path(secondProjDataPath, "dataset", "processed.csv"))
  
  # ------- CALL FUNCTION UNDER TEST ------
  
  outputFile <- str_c(tempfile(), ".zip")
  downloadProcessedDataAsZip("first project", outputFile, basePath)

  # ------- MAKE SURE OUTPUT IS CORRECT -------
  
  expect_true(file.exists(outputFile))
  unzipDir <- file.path(tempdir(), "unzipDir")
  on.exit(unlink(unzipDir, recursive = TRUE))
  
  unzip(outputFile, exdir = unzipDir)
  
  files <- list.files(unzipDir)
  expect_equal(files, c("dataset 1.csv", "dataset2.csv"))
  expect_equal(
    read_csv(file.path(unzipDir, "dataset 1.csv")),
    df1
  )
  expect_equal(
    read_csv(file.path(unzipDir, "dataset2.csv")),
    df2
  )
  
})

test_that("test downloading one file with all processed data", {
  
  # -------- INITIALIZE INPUTS -------
  
  basePath <- file.path(tempdir(), "testDownloadAllDataSingleFile")
  dir.create(basePath, recursive = TRUE)
  on.exit(unlink(basePath, recursive = TRUE))
  
  firstProjDataPath <- file.path(basePath, "first project", "data")
  
  dir.create(file.path(firstProjDataPath, "dataset 1"), recursive = TRUE)
  df1 <- tibble(a = c(1.2, 3.4), b = c("a", "b"))
  write_csv(df1, file.path(firstProjDataPath, "dataset 1", "processed.csv"))
  
  dir.create(file.path(firstProjDataPath, "dataset2.csv"), recursive = TRUE)
  df2 <- tibble(a = c(4.6, 7.8), b = c("x", "y"))
  write_csv(df2, file.path(firstProjDataPath, "dataset2.csv", "processed.csv"))
  
  dir.create(file.path(firstProjDataPath, "dataset 3"), recursive = TRUE)
  
  # create project, whose data should not be downloaded
  secondProjDataPath <- file.path(basePath, "second project", "data")
  dir.create(file.path(secondProjDataPath, "dataset"), recursive = TRUE)
  write_csv(tibble(x = c(1,2,3)), file.path(secondProjDataPath, "dataset", "processed.csv"))
  
  # -------- CALL FUNCTION UNDER TEST -------
  
  outputFile <- str_c(tempfile(), ".csv")
  downloadProcessedDataSingleFile("first project", outputFile, basePath)
  
  # ------- MAKE SURE OUTPUT IS CORRECT -------
  
  output <- read_csv(outputFile)
  
  expect_is(output, "data.frame")
  expect_equal(
    output,
    tibble(
      dataset = c("dataset 1", "dataset 1", "dataset2.csv", "dataset2.csv"),
      a = c(1.2, 3.4, 4.6, 7.8), 
      b = c("a", "b", "x", "y")
    )
  )
})