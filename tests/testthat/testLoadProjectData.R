library(testthat)
library(readr)

context("test loading project data on page 'Project'")

test_that("test", {
  
  # --------- SETUP TEST PROJECT ------------
  
  project <- "Project A"
  basePath <- file.path(tempdir(), "testLoadProjectData")
  dir.create(basePath, recursive = TRUE)
  on.exit(unlink(basePath, recursive = TRUE))
  
  # basePath
  #   |_ Project A
  #      |_ data
  #         |_ dataset 1
  #            |_ raw.csv
  #            |_ processed.csv
  #            |_ fileInfo.txt
  #            |_ processingOptions.csv
  #            |_ sampleDescription.csv
  #         |_ dataset 2
  #            |_ bcdef45678.csv (raw)
  #            |_ fileInfo.txt
  #         |_ dataset 3
  #            |_ raw.csv
  #         |_ dataset IV
  #            |_ processingOptions.csv
  #            |_ sampleDescription.csv
  #         
  
  dataPath <- file.path(basePath, project, "data")
  dir.create(dataPath, recursive = TRUE)
  
  dataset1Path <- file.path(dataPath, "dataset 1")
  dir.create(dataset1Path)
  write_csv(tibble(TimeCode = c("2015/11/2504:47:06", "2015/11/2512:50:10")), 
            file.path(dataset1Path, "raw.csv"))
  write_file("some info", file.path(dataset1Path, "fileInfo.txt"))
  file.create(file.path(dataset1Path, "processed.csv"))
  file.create(file.path(dataset1Path, "processingOptions.csv"))
  file.create(file.path(dataset1Path, "sampleDescription.csv"))
  
  
  dataset2Path <- file.path(dataPath, "dataset 2")
  dir.create(dataset2Path)
  write_csv(tibble(TimeCode = c("1990/07/0304:47:06", "1990/07/0312:50:10")), 
            file.path(dataset2Path, "bcdef45678.csv"))
  write_file("additional info yay", file.path(dataset2Path, "fileInfo.txt"))
  
  dataset3Path <- file.path(dataPath, "dataset 3")
  dir.create(dataset3Path)
  write_csv(tibble(TimeCode = c("2019/01/3104:47:06", "2019/01/3112:50:10")), 
            file.path(dataset3Path, "raw.csv"))
  
  dataset4Path <- file.path(dataPath, "dataset IV")
  dir.create(dataset4Path)
  file.create(file.path(dataset4Path, "sampleDescription.csv"))
  file.create(file.path(dataset4Path, "processingOptions.csv"))
  
  # -------------- SET EXPECTED RESULT ---------------
  
  projectDataExpected <- list(
    `dataset 1` = list(
      path = file.path(basePath, project, "data", "dataset 1"),
      raw = "raw.csv",
      date = "2015-11-25",
      additionalInfo = "some info",
      processed = "processed.csv"
    ),
    `dataset 2` = list(
      path = file.path(basePath, project, "data", "dataset 2"),
      raw = "bcdef45678.csv",
      date = "1990-07-03",
      additionalInfo = "additional info yay"
    ),
    `dataset 3` = list(
      path = file.path(basePath, project, "data", "dataset 3"),
      raw = "raw.csv",
      date = "2019-01-31"
    ),
    `dataset IV` = list(
      path = file.path(basePath, project, "data", "dataset IV")
    )
  )
  
  # ---------------- RUN FUNCTION loadProjectData -------------------------
  
  projectDataActual <- loadProjectData(project, basePath)
  
  # ---------------- COMPARE RESULT TO EXPECTED --------------------
  
  expect_equal(projectDataActual, projectDataExpected)
})