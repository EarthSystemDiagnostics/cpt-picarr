library(testthat)
library(tidyverse)

context("test the function validateSampleDescrAndProccOptions")

# --------- INITIALIZE SHARED VALUES --------

processingOptions <- system.file("tests", "testthat", "test_data", 
                                 "processingOptions.csv", package = "cptPicarr") %>%
  read_csv()

sampleDescription <- system.file("tests", "testthat", "test_data", 
                                 "sampleDescription.csv", package = "cptPicarr") %>%
  read_csv()


test_that("download button should be displayed", {
  
  # --------- CALL FUNCTION UNDER TEST ----------
  
  actual <- validateSampleDescrAndProccOptions(sampleDescription, processingOptions, id = "abc")
  
  # --------- MAKE EXPECTATIONS ----------------
  
  expect_is(actual, "shiny.tag")
  expect_equal(actual$attribs$id, "abc")
  expect_equal(actual$attribs$class, "btn btn-default shiny-download-link ")
  expect_equal(actual$children[[2]], "Download sample description")
  
})

test_that("standard missing from processing options", {
  
  # --------- MODIFY INPUTS -----------
  
  # remove last row
  .processingOptions <- slice(processingOptions, -n())
  
  # --------- CALL FUNCTION UNDER TEST ----------
  
  actual <- validateSampleDescrAndProccOptions(sampleDescription, .processingOptions, id = "abc")
  
  # --------- MAKE EXPECTATIONS ----------------
  
  expect_is(actual, "shiny.tag")
  expect_equal(
    actual$children, 
    list("Input error: The standard '", "NGT", "' is missing from the processing options.")
  )
})

test_that("extra standard in processing options", {
  
  # --------- MODIFY INPUTS -----------
  
  .processingOptions <- add_row(processingOptions, `Identifier 1` = "Extra")
  
  # --------- CALL FUNCTION UNDER TEST ----------
  
  actual <- validateSampleDescrAndProccOptions(sampleDescription, .processingOptions, id = "abc")
  
  # --------- MAKE EXPECTATIONS ----------------
  
  expect_is(actual, "shiny.tag")
  expect_equal(
    actual$children, 
    list("Input error: The standard '", "Extra", 
         "' is included in the processing options but not in the sample description.")
  )
})

test_that("NA in processing options", {
  
  # --------- MODIFY INPUTS -----------
  
  .processingOptions <- processingOptions
  .processingOptions[3,3] <- NA
  .processingOptions[4,5] <- NA
  
  # --------- CALL FUNCTION UNDER TEST ----------
  
  actual <- validateSampleDescrAndProccOptions(sampleDescription, .processingOptions, id = "abc")
  
  # --------- MAKE EXPECTATIONS ----------------
  
  expect_is(actual, "shiny.tag")
  expect_equal(
    actual$children, 
    list("Input error: The processing options table contains empty cells.")
  )
})

test_that("control standard may not be selected for anything else", {
  
  # --------- MODIFY INPUTS -----------
  
  .processingOptions <- processingOptions
  .processingOptions[3, "Use as control standard"] <- TRUE
  
  # --------- CALL FUNCTION UNDER TEST ----------
  
  actual <- validateSampleDescrAndProccOptions(sampleDescription, .processingOptions, id = "abc")
  
  # --------- MAKE EXPECTATIONS ----------------
  
  expect_is(actual, "shiny.tag")
  expect_equal(
    actual$children, 
    list("Input error: The standard '", "TD1", 
         "' is selected as control standard. It may not be selected for anything else.")
  )
})

test_that("processing options ID1 contains duplicates", {
  
  # --------- MODIFY INPUTS -----------
  
  .processingOptions <- add_row(processingOptions, `Identifier 1` = "TD1")
  
  # --------- CALL FUNCTION UNDER TEST ----------
  
  actual <- validateSampleDescrAndProccOptions(sampleDescription, .processingOptions, id = "abc")
  
  # --------- MAKE EXPECTATIONS ----------------
  
  expect_is(actual, "shiny.tag")
  expect_equal(
    actual$children, 
    list("Input error: The processing options column 'Identifier 1' contains duplicates.")
  )
})

test_that("sample description is NULL", {
  
  # --------- CALL FUNCTION UNDER TEST ----------
  
  actual <- validateSampleDescrAndProccOptions(NULL, processingOptions, id = "abc")
  
  # --------- MAKE EXPECTATIONS ----------------
  
  expect_is(actual, "shiny.tag")
  expect_equal(
    actual$children, 
    list("Sample description is empty.")
  )
})

test_that("processing options table is NULL", {
  
  # --------- CALL FUNCTION UNDER TEST ----------
  
  actual <- validateSampleDescrAndProccOptions(sampleDescription, NULL, id = "abc")
  
  # --------- MAKE EXPECTATIONS ----------------
  
  expect_is(actual, "shiny.tag")
  expect_equal(
    actual$children, 
    list("Processing options table is empty.")
  )
})