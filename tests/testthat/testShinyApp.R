library(shinytest)

context("test the application in the app/ directory")


test_that("cpt picarr works", {
  
  expect_pass(testApp("app/", compareImages = FALSE))
})