library(testthat)

context("test the function calculateOverallUncertainty")

test_that("test normal case", {
  
  processedData <- list(
    list(
      list(
        deviationOfControlStandard = list(d18O = 1, dD = 2)
      )
    ),
    list(
      list(
        deviationOfControlStandard = list(d18O = 2, dD = 4)
      )
    )
  )
  
  actual <- calculateOverallUncertainty(processedData)
  
  expect_is(actual, "data.frame")
  expect_equal(round(actual$d18O, 6), 1.581139)
  expect_equal(round(actual$dD, 6), 3.162278)
})