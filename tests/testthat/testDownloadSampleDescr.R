library(testthat)
library(readr)
library(tibble)

context("test function downloadSampleDescr")

test_that("test", {
  
  data <- tribble(
    ~`Identifier 1`, ~`Identifier 2`, ~Tray, ~colA,
    "abc",           "",              1,     "x",
    "xyz",           "abc",           2,     4 
  )
  dataExpected <- tribble(
    ~`Rack Pos.`, ~`Identifier 1`, ~`Identifier 2`,       ~Tray,
    1,            "abc",           "_3456ftjzgk.4536",    1,
    2,            "xyz",           "abc_3456ftjzgk.4536", 2
  )
  file <- file.path(tempdir(), "outputDownloadSampleDescr.csv")
  uniqueIdentifier <- "3456ftjzgk.4536"
  on.exit(file.remove(file))
  
  downloadSampleDescr(data, file, uniqueIdentifier)
  
  expect_true(file.exists(file))
  expect_equal(dataExpected, read_csv(file))
})