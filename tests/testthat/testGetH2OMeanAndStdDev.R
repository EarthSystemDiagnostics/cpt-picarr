library(testthat)
library(tidyverse)

data <- tribble(
  ~Line, ~`Identifier 1`, ~block, ~H2O_Mean,
  # -- | ------------ | ------ | ----------
  1, "A", 1, 1,
  2, "A", 1, 2,
  3, "A", 1, 6,
  4, "B", NA, 6,
  5, "B", NA, 10,
  6, "B", NA, 12,
  7, "B", NA, 100,
  8, "A", 2, 0,
  9, "A", 2, 4,
  10, "A", 2, 7,
  11, "A", 2, 10
)

test_that("test getH2OMeanAndStdDev (nInj = 'all')", {
  
  expected <- tribble(
    ~Line, ~`Identifier 1`, ~block, ~H2OMean, ~H2OSD,
    # -- | -------------- | ----- | ------- | -------
    1, "A", 1, 3, 2.65,
    2, "B", NA, 32, 45.4,
    3, "A", 2, 5.25, 4.27
  )
  
  actual <- getH2OMeanAndStdDev(data, "all")
  actual <- mutate(actual, H2OSD = round(H2OSD, 2))
  
  expect_equal(actual, expected)
})

test_that("test getH2OMeanAndStdDev (nInj = 3)", {
  
  expected <- tribble(
    ~Line, ~`Identifier 1`, ~block, ~H2OMean, ~H2OSD,
    # -- | -------------- | ----- | ------- | -------
    1, "A", 1, 3, 2.65,
    2, "B", NA, 40.67, 51.39,
    3, "A", 2, 7, 3
  )
  
  actual <- getH2OMeanAndStdDev(data, 3)
  actual <- mutate(actual, H2OSD = round(H2OSD, 2),  H2OMean = round(H2OMean, 2))
  
  expect_equal(actual, expected)
})