library(testthat)
library(readr)
library(rlist)

context("test loading project data on page 'Project'")

test_that("test", {
  
  # --------- SETUP TEST PROJECT ------------
  
  project <- "Project A"
  basePath <- file.path(tempdir(), "testLoadProjectData")
  dir.create(basePath, recursive = TRUE)
  on.exit(unlink(basePath, recursive = TRUE))
  
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

  # -------------- SET EXPECTED RESULT ---------------
  
  projectDataExpected <- list(
    `dataset 1` = list(
      path = file.path(basePath, project, "data", "dataset 1"),
      raw = "raw.csv",
      additionalInfo = "some info",
      date = "2015-11-25",
      device = "device A",
      processed = "processed.csv"
    ),
    `dataset 2` = list(
      path = file.path(basePath, project, "data", "dataset 2"),
      raw = "bcdef45678.csv",
      additionalInfo = "additional info yay",
      date = "1990-07-03",
      device = "device B"
    ),
    `dataset 3` = list(
      path = file.path(basePath, project, "data", "dataset 3"),
      raw = "raw.csv",
      additionalInfo = "",
      date = "2019-01-31",
      device = ""
    )
  )
  
  # ---------------- RUN FUNCTION loadProjectData -------------------------
  
  projectDataActual <- loadProjectData(project, basePath)

  # ---------------- COMPARE RESULT TO EXPECTED --------------------
  
  expect_equal(projectDataActual, projectDataExpected)
})