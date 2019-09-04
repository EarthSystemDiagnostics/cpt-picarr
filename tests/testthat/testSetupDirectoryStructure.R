library(testthat)

test_that("test setting up the directory struture", {
  
  basePath <- file.path(tempdir(), "testSetupDirStructure")
  
  # make sure the directory basePath does not exist
  unlink(basePath, recursive = TRUE)
  
  setupDirectoryStructure(basePath)
  
  expect_true(dir.exists(basePath))
  expect_true(dir.exists(file.path(basePath, "processingOptions")))
})