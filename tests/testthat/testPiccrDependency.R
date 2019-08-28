context("test that piccr functions are accessible in cpt-picarr")

test_that("test piccr loading", {
  expect_type(piccr::processData, "closure")
})