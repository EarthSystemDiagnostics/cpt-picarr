library(testthat)
library(rlist)

context("test creating a new project")

test_that("test projectExistsAlready", {
  
  basePath <- file.path(tempdir(), "testProjectCreation1")
  dir.create(basePath)
  dir.create(file.path(basePath, "Project A"))
  on.exit(unlink(basePath, recursive = TRUE))
  
  expect_true(projectExistsAlready("Project A", basePath = basePath))
  expect_false(projectExistsAlready("Project B", basePath = basePath))
})

test_that("test createProjectDirectory", {
  
  basePath <- file.path(tempdir(), "testProjectCreation2")
  dir.create(basePath)
  on.exit(unlink(basePath, recursive = TRUE))
  
  createProjectDirectory("Project A", basePath = basePath)
  
  expect_true(dir.exists(file.path(basePath, "Project A")))
  
})

test_that("test crateProjectInfoFile", {
  
  basePath <- file.path(tempdir(), "testProjectCreation3")
  dir.create(basePath)
  dir.create(file.path(basePath, "Project A"))
  on.exit(unlink(basePath, recursive = TRUE))
  
  input <- list(
    projectName = "Project A",
    projectPeople = "Max Mustermann",
    projectAdditionalInfo = "some additional \n info.",
    projectDate = as.Date("2019-09-06")
  )
  
  infoExpected <- list(
    name = "Project A",
    people = "Max Mustermann",
    additionalInfo = "some additional \n info.",
    date = "2019-09-06"
  )
  
  crateProjectInfoFile(input, basePath = basePath)
  
  filePath <- file.path(basePath, "Project A", "projectInfo.json")
  expect_true(file.exists(filePath))
  expect_equal(list.load(filePath), infoExpected)
})

test_that("test crateProjectInfoFile (date is NA)", {
  
  basePath <- file.path(tempdir(), "testProjectCreation4")
  dir.create(basePath)
  dir.create(file.path(basePath, "Project A"))
  on.exit(unlink(basePath, recursive = TRUE))
  
  input <- list(
    projectName = "Project A",
    projectPeople = "Max Mustermann",
    projectAdditionalInfo = "some additional \n info.",
    projectDate = NA
  )
  
  infoExpected <- list(
    name = "Project A",
    people = "Max Mustermann",
    additionalInfo = "some additional \n info.",
    date = NA
  )
  
  crateProjectInfoFile(input, basePath = basePath)
  
  filePath <- file.path(basePath, "Project A", "projectInfo.json")
  expect_true(file.exists(filePath))
  expect_equal(list.load(filePath), infoExpected)
})