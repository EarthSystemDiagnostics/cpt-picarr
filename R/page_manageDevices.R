library(shiny)
library(tidyverse)
library(rlist)

pageManageDevicesUI <- function(id){
  
  # create namespace function
  ns <- NS(id)
  
  tagList(
    h2("Manage devices"),
    
    wellPanel(
      style = "height: 550px",
      h3("Known devices"),
      conditionalPanel("true",
        # add CSS to make panel scrollable to reduce vertical size
        style = "overflow-y:scroll; height: 435px",
        uiOutput(ns("knownDevices"))
      )
    ),
    
    wellPanel(
      h3("Add a new device"),
      textInput(ns("deviceName"), "Name"),
      textInput(ns("deviceCode"), "File name code (serial number)"),
      textAreaInput(ns("deviceInfo"), "Additional info (optional)"),
      actionButton(ns("doAddDevice"), "Add device", style = blue),
      textOutput(ns("helpMessage"))
    ),
    
    # silently pass the given id to the server function
    conditionalPanel("false", textInput(ns("id"), label = "", value = id))
  )
}

pageManageDevices <- function(input, output, session, serverEnvironment){
  
  # create a namespace function using the id passed by the frontend
  ns <- reactive(NS(isolate(input$id)))
  
  rv <- reactiveValues()
  rv$devicesUpdated <- 1
  
  # ------------ DISPLAY KNOWN DEVICES -------
  
  output$knownDevices <- renderUI({
    
    force(rv$devicesUpdated)
    
    devices <- getDevices()
    devicesOutput <- map(devices, ~ {
      wellPanel(
        p(strong("Name: "), .$name),
        p(strong("File name code: "), .$code),
        p(strong("Additional info: "), .$info)
      )
    })
    tagList(devicesOutput)
  })
  
  # ------------ ADD NEW DEVICE ---------------
  
  observeEvent(input$doAddDevice, {
    req(input$deviceName, input$deviceCode)
    message <- addDevice(
      name = input$deviceName, code = input$deviceCode, info = input$deviceInfo)
    output$helpMessage <- renderText(message)
    rv$devicesUpdated <- rv$devicesUpdated + 1
  })
  
}

#####################
# HELPERS
#####################

addDevice <- function(name, code, info, basePath = BASE_PATH){
  
  # validate input
  if (nameInUse(name, basePath))
    return("Input Error: The chosen name is already in use.")
  if (codeInUse(code, basePath))
    return("Input Error: The chosen code is already in use.")
  
  path      <- file.path(basePath, "devices.json")
  newDevice <- list(name = name, code = code, info = info)
  
  if (file.exists(path)){
    oldDevices <- list.load(path)
    newDevices <- list.append(oldDevices, newDevice)
  } else {
    newDevices <- list(newDevice)
  }
  list.save(newDevices, path)
  
  return("Device successfully added.")
}

nameInUse <- function(name, basePath){
  
  devices <- getDevices(basePath)
  name %in% map_chr(devices, ~ .$name)
}

codeInUse <- function(code, basePath){
  
  devices <- getDevices(basePath)
  code %in% map_chr(devices, ~ .$code)
}