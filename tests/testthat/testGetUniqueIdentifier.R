library(testthat)

context("test function generateUniqueIdentifier")

test_that("test", {
  
  data1 <- tribble(
    ~colA, ~colB,
    1,     2
  )
  data2 <- tribble(
    ~colA, ~colB,
    3,     4
  )
  
  id1 <- generateUniqueIdentifer(data1)
  id2 <- generateUniqueIdentifer(data2)
  
  expect_type(id1, "character")
  expect_true(id1 != id2)
})