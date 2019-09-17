library(testthat)

context("test function getDataForDevicesAndTimespan")

test_that("test", {
  
  # ----------- INITIALIZE INPUTS --------
  
  basePath <- file.path(tempdir(), "testGetDataForDevices")
  dir.create(basePath, recursive = TRUE)
  on.exit(unlink(basePath, recursive = TRUE))
  
  dataPath <- file.path(basePath, "project A", "data")
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
  
  dataset3Path <- file.path(basePath, "project B", "data", "dataset 3")
  dir.create(dataset3Path, recursive = TRUE)
  write_csv(tibble(col = c(1,2,3)), file.path(dataset3Path, "raw.csv"))
  file.create(file.path(dataset3Path, "processed.csv"))
  list.save(
    list(additionalInfo = "", date = "2019-01-31", device = "device A"),
    file.path(dataset3Path, "fileInfo.json"))
  
  # -------- CALL FUNCTION UNDER TEST -------------
  
  data <- getDataForDevicesAndDaterange(c("device A"), "2017-01-01", "2020-01-01", basePath)
  
  # -------- MAKE EXPECTATIONS -------------
  
  expect_equal(data, c(dataset3Path))
})