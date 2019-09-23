library(testthat)
library(readr)
library(tibble)

context("test function downloadSampleDescr")

test_that("test", {
  
  data <- tribble(
    ~`Identifier 1`, ~`Identifier 2`, ~tray, ~colA,
    "abc",           1,              1,     "x",
    "xyz",           NA_real_,           2,     4 
  )
  dataExpected <- tribble(
    ~vial, ~`Identifier 1`, ~`Identifier 2`,       ~tray,
    1,            "abc",           "1_3456ftjzgk.4536",    1,
    2,            "xyz",           "_3456ftjzgk.4536", 2
  )
  file <- file.path(tempdir(), "outputDownloadSampleDescr.csv")
  uniqueIdentifier <- "3456ftjzgk.4536"
  on.exit(file.remove(file))
  
  downloadSampleDescr(data, file, uniqueIdentifier)
  
  expect_true(file.exists(file))
  expect_equal(dataExpected, read_csv(file))
})