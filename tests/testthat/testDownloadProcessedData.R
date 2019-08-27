library(testthat)
library(tibble)

context("test downloading processed data on the page 'process data'")

df1 <- tribble(
  ~col1, ~col2,
  1,     2,
  "a",   "b"
)
df2 <- tribble(
  ~colA, ~colB,
  8,     5,
  190,   "x"
)
df3 <- tribble(
  ~colA, ~colB,
  800,   500,
  19000, "x00"
)
outputFile <- "test_download.zip"

test_that("test download (1)", {
  
  processedData <- list(df1 = df1, df2 = df2)
  
  downloadProcessedData(outputFile, processedData)
  
  expect_true(file.exists(
    file.path(tempdir(), outputFile)
  ))
  
  filesInZip <- unzip(file.path(tempdir(), outputFile), list = TRUE)
  
  expect_equal(nrow(filesInZip), 2)
  expect_equal(filesInZip$Name, c("df1", "df2"))
  expect_equal(filesInZip$Length, c(18, 20))
})

test_that("test download (2)", {
  
  processedData <- list(df1 = df1, df3 = df3)
  
  downloadProcessedData(outputFile, processedData)
  
  expect_true(file.exists(
    file.path(tempdir(), outputFile)
  ))
  
  filesInZip <- unzip(file.path(tempdir(), outputFile), list = TRUE)
  
  expect_equal(nrow(filesInZip), 2)
  expect_equal(filesInZip$Name, c("df1", "df3"))
  expect_equal(filesInZip$Length, c(18, 28))
})