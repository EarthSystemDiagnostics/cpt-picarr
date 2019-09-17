library(shiny)
library(tidyverse)
library(rlist)

#' pageManageDevicesUI
#'
#' UI function for the page 'Manage devices'.
#'
#' @param id Identifier for the namespace of this module
#'
#' @return A HTML tag object
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

#' pageManageDevices
#'
#' Implements the server logic for the page 'Manage devices'.
#'
#' @param input Shiny inputs
#' @param output Shiny outputs
#' @param session Shiny session
#' @param serverEnvironment An environment. The environment of the 
#'                          server function that calls this module.
#'                          Used to execute code in the environment of the 
#'                          main server function for the app (e.g. to 
#'                          switch between pages).
#'
#' @return No explicit return value
pageManageDevices <- function(input, output, session, serverEnvironment){
  
  # ------------ INITIALIZE REACTIVE STATE ----------
  
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

#' addDevice
#'
#' Add a new measurement device to the collection of known devices.
#' Manipulates the file 'BASE_PATH/devices.json'.
#'
#' @return A status message that can be rendered as text output.
addDevice <- function(name, code, info, basePath = BASE_PATH){
  
  # exit early if name or code is already in use
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