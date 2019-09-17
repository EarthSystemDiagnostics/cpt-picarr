library(testthat)
library(rlist)

test_that("test add device (devices.json exists)", {
  
  basePath <- file.path(tempdir(), "testAddDevice")
  dir.create(basePath)
  on.exit(unlink(basePath, recursive = TRUE))
  
  originalList <- list(
    name = "device name",
    code = "code 123",
    info = "some additional info"
  )
  list.save(
    list(originalList),
    file.path(basePath, "devices.json")
  )
  
  message <- addDevice(name = "name 2", code = "code 2", info = "info 2", basePath = basePath)
  
  expect_equal(
    list.load(file.path(basePath, "devices.json")),
    list(originalList, list(name = "name 2", code = "code 2", info = "info 2"))
  )
  expect_equal(message, "Device successfully added.")
})

test_that("test add device (devices.json does not exist)", {
  
  basePath <- file.path(tempdir(), "testAddDevice2")
  dir.create(basePath)
  on.exit(unlink(basePath, recursive = TRUE))
  
  message <- addDevice(name = "name 1", code = "code 1", info = "info 1", basePath = basePath)
  
  expect_equal(
    list.load(file.path(basePath, "devices.json")),
    list(list(name = "name 1", code = "code 1", info = "info 1"))
  )
  expect_equal(message, "Device successfully added.")
})

test_that("test add device (device with same name exists already)", {
  
  basePath <- file.path(tempdir(), "testAddDevice")
  dir.create(basePath)
  on.exit(unlink(basePath, recursive = TRUE))
  
  originalList <- list(
    name = "device name",
    code = "code 123",
    info = "some additional info"
  )
  list.save(
    list(originalList),
    file.path(basePath, "devices.json")
  )
  
  message <- addDevice(name = "device name", code = "code 2", info = "info 2", basePath = basePath)
  
  expect_equal(
    list.load(file.path(basePath, "devices.json")),
    list(originalList)
  )
  expect_equal(message, "Input Error: The chosen name is already in use.")
})

test_that("test add device (device with same code exists already)", {
  
  basePath <- file.path(tempdir(), "testAddDevice")
  dir.create(basePath)
  on.exit(unlink(basePath, recursive = TRUE))
  
  originalList <- list(
    name = "device name",
    code = "code 123",
    info = "some additional info"
  )
  list.save(
    list(originalList),
    file.path(basePath, "devices.json")
  )
  
  message <- addDevice(name = "name 2", code = "code 123", info = "info 2", basePath = basePath)
  
  expect_equal(
    list.load(file.path(basePath, "devices.json")),
    list(originalList)
  )
  expect_equal(message, "Input Error: The chosen code is already in use.")
})

test_that("test getDevices (devices.json exists)", {
  
  basePath <- file.path(tempdir(), "testGetDevices")
  dir.create(basePath)
  on.exit(unlink(basePath, recursive = TRUE))
  
  devices <- list(
    list(
      name = "a",
      code = "b",
      info = "c"
    ),
    list(
      name = "1",
      code = "2",
      info = ""
    )
  )
  list.save(devices, file.path(basePath, "devices.json"))
  
  actual <- getDevices(basePath)
  
  expect_equal(actual, devices)
})

test_that("test getDevices (devices.json does not exist)", {
  
  basePath <- file.path(tempdir(), "testGetDevices2")
  dir.create(basePath)
  on.exit(unlink(basePath, recursive = TRUE))

  actual <- getDevices(basePath)
  
  expect_equal(actual, list())
})