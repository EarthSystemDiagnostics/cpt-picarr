library(shiny)
library(rhandsontable)
library(tidyverse)
library(futile.logger)

pageGenerateSampleDescrUI <- function(id){
  
  # create namespace function
  ns <- NS(id)
  
  tagList(
    h2("Generate a sample description file"), br(),
    
    p("Click this button before doing anything else:"),
    actionButton(ns("loadTemplates"), "Load templates for the selected project", style = blue), p(""), br(),
    
    wellPanel(
      h3("Create a sample description"),
      selectInput(ns("selectTemplateSampleDescr"), "Select a template", c()), p(""), br(),
      rHandsontableOutput(ns("hotSampleDescr")), br(),
      actionButton(ns("addRowSampleDescr"), "+ Add a row"), "(Right click table for more options)", p(""), 
      hr(),
      h4("Save as new template"),
      textInput(ns("templateNameSampleDescr"), "Name the template"),
      actionButton(ns("saveNewTemplateSampleDescr"), "Save as new template"),
      textOutput(ns("helpMessageSampleDescr"), inline = TRUE)
    ),
    
    wellPanel(
      h3("Which standard should be used for what?"),
      selectInput(ns("selectTemplateProcessing"), "Select a template", c()), p(""), br(),
      rHandsontableOutput(ns("hotProcessingOptions")), br(),
      actionButton(ns("addRowProcessing"), "+ Add a row"), "(Right click table for more options)", p(""), 
      hr(),
      h4("Save as new template"),
      textInput(ns("templateNameProcessing"), "Name the template"),
      actionButton(ns("saveNewTemplateProcessing"), "Save as new template"),
      textOutput(ns("helpMessageProcessing"), inline = TRUE)
    ),
    
    wellPanel(
      h3("All done?"),
      downloadButton(ns("download"), "Download sample description", style = blue)
    )
  )
}

# project is a reactive value
pageGenerateSampleDescr <- function(input, output, session, project){
  
  observeEvent(input$loadTemplates, {
    updateTemplateSelectionListSampleDescr(session, NULL, project())
    updateTemplateSelectionListProcessing(session, NULL, project())
  })
  
  
  # -------- REACTIVE VALUES -----------------
  
  rv <- reactiveValues()
  rv$sampleDescr <- emptySampleDescr
  rv$processingOptions <- processingOptionsInitial
  
  # Used to match measurement data with its sample description and processing template.
  # Is appended to Identifer 2 to preserve through the measurement process.
  rv$uniqueIdentifier <- NULL
  
  # -------- SAMPLE DESCRIPTION ----------------
  
  observeEvent(input$selectTemplateSampleDescr, {
    templateName <- input$selectTemplateSampleDescr
    template <- getDataForTemplateSampleDescr(templateName, project())
    rv$sampleDescr <- template
  })
  
  output$hotSampleDescr <- renderRHandsontable({
    buildHandsontableSampleDescr(rv$sampleDescr)
  })
  
  observeEvent(input$addRowSampleDescr, {
    data <- hot_to_r(input$hotSampleDescr)
    rv$sampleDescr <- add_row(data, Tray = 1)
  })
  
  observeEvent(input$saveNewTemplateSampleDescr, {
    
    name <- input$templateNameSampleDescr
    data <- hot_to_r(input$hotSampleDescr)
    
    helpMessage <- saveNewTemplate(data, name, project(), "sampleDescription")
    output$helpMessageSampleDescr <- renderText(helpMessage)
    
    updateTemplateSelectionListSampleDescr(session, name, project())
  })

  # -------- PROCESSING ----------------
  
  observeEvent(input$selectTemplateProcessing, {
    templateName <- input$selectTemplateProcessing
    template <- getDataForTemplateProcessing(templateName, project())
    rv$processingOptions <- template
  })
  
  output$hotProcessingOptions <- renderRHandsontable({
    buildHandsontableProcessing(rv$processingOptions)
  })
  
  observeEvent(input$addRowProcessing, {
    data <- hot_to_r(input$hotProcessingOptions)
    rv$processingOptions <- add_row(data)
  })
  
  observeEvent(input$saveNewTemplateProcessing, {
    
    name <- input$templateNameProcessing
    data <- hot_to_r(input$hotProcessingOptions)
    
    helpMessage <- saveNewTemplate(data, name, project(), "processing")
    output$helpMessageProcessing <- renderText(helpMessage)
    
    updateTemplateSelectionListProcessing(session, name, project())
  })
  
  # -------- DOWNLOAD AND SAVE ----------------
  
  output$download <- downloadHandler(
    filename = "sample_description.csv",
    content = function(file) {
      
      data <- rv$sampleDescr
      rv$uniqueIdentifier <- getUniqueIdentifer(data)
      
      downloadSampleDescr(data, file, rv$uniqueIdentifier)
    }
  )
  
  observeEvent(rv$uniqueIdentifier, {
    
    sampleDescr <- rv$sampleDescr
    processingOptions <- hot_to_r(input$hotProcessingOptions)
    uniqueIdentifier <- rv$uniqueIdentifier
    
    saveOnServer(sampleDescr, processingOptions, uniqueIdentifier)
  })
}

######################################
# HELPERS 
######################################

#' @return A help message to be displayed next to the save button
saveNewTemplate <- function(data, name, project, mode = "sampleDescription", basePath = BASE_PATH){
  
  if (name == "") return("Please enter a name for the template")
  
  templateDir <- file.path(basePath, project, "templates", mode)
  file <- file.path(templateDir, name)
  
  flog.debug(sprintf("saving new template to %s", file))
  
  dir.create(templateDir, recursive = TRUE)
  write_csv(data, file)
  return("Template successfully saved.")
}

updateTemplateSelectionListSampleDescr <- function(session, selected, project){
  templates <- list.files(file.path(BASE_PATH, project, "templates", "sampleDescription"))
  updateSelectInput(session, "selectTemplateSampleDescr", 
                    choices = c("Empty sample description", templates),
                    selected = selected)
}

updateTemplateSelectionListProcessing <- function(session, selected, project){
  templates <- list.files(file.path(BASE_PATH, project, "templates", "processing"))
  updateSelectInput(session, "selectTemplateProcessing", 
                    choices = c("Empty processing template", templates),
                    selected = selected)
}

getDataForTemplateSampleDescr <- function(templateName, project) {
  if (templateName %in% c("", "Empty sample description")){
    template <- emptySampleDescr
  } else {
    file <- file.path(BASE_PATH, project, "templates", "sampleDescription", templateName)
    template <- read_csv(file)
  }
  return(template)
}

getDataForTemplateProcessing <- function(templateName, project) {
  if (templateName %in% c("", "Empty processing template")){
    template <- processingOptionsInitial
  } else {
    file <- file.path(BASE_PATH, project, "templates", "processing", templateName)
    template <- read_csv(file)
  }
  return(template)
}

buildHandsontableSampleDescr <- function(data){
  rhandsontable(data) %>%
    hot_col(col = "Identifier 1", type = "text") %>%
    hot_col(col = "Identifier 2", type = "text") %>%
    hot_col(col = "Is standard?", type = "checkbox") %>%
    hot_col(col = "Tray", type = "dropdown", source = 1:2)
}

buildHandsontableProcessing <- function(data){
  rhandsontable(data) %>%
    hot_col(col = "Identifier 1", type = "text") %>%
    hot_col(col = "Use for drift correction", type = "checkbox") %>%
    hot_col(col = "Use for calibration", type = "checkbox") %>%
    hot_col(col = "Use as control standard", type = "checkbox") %>%
    hot_col(col = "True delta O18", type = "numeric") %>%
    hot_col(col = "True delta H2", type = "numeric")
}

getUniqueIdentifer <- function(data){
  sampleDescrHash <- digest::digest(data)  # create an md5 hash of the sample description
  timestamp <- as.numeric(Sys.time())
  str_c(sampleDescrHash, timestamp)
}

downloadSampleDescr <- function(data, file, uniqueIdentifier){
  
  flog.info("Dowloading sample description")
  flog.debug(str_c("Unique identifier: ", uniqueIdentifier))
  
  data <- data %>%
    select(`Identifier 1`, `Identifier 2`, `Tray`) %>%
    mutate(`Identifier 2` = str_replace_na(`Identifier 2`, replacement = "")) %>%
    mutate(`Identifier 2` = str_c(`Identifier 2`, "_", uniqueIdentifier)) %>%
    rowid_to_column("Rack Pos.")
  
  write_csv(data, file)
}

saveOnServer <- function(sampleDescr, processingOptions, uniqueIdentifier, basePath = BASE_PATH){
  
  path <- file.path(basePath, "processingOptions", uniqueIdentifier)
  
  dir.create(path, recursive = TRUE)
  write_csv(sampleDescr, file.path(path, "sampleDescription.csv"))
  write_csv(processingOptions, file.path(path, "processingOptions.csv"))
}

######################################
# ARTEFACTS 
######################################

processingOptionsInitial <- tribble(
  ~`Identifier 1`, ~`Use for drift correction`, ~`Use for calibration`, ~`Use as control standard`, ~`True delta O18`, ~`True delta H2`,
  # ------------ / -------------------------- / --------------------- / ------------------------- / ---------------- / ----------------
  "",              FALSE,                          FALSE,                    FALSE,                 NA_real_,          NA_real_,
  "",              FALSE,                          FALSE,                    FALSE,                 NA_real_,          NA_real_,
  "",              FALSE,                          FALSE,                    FALSE,                 NA_real_,          NA_real_,
  "",              FALSE,                          FALSE,                    FALSE,                 NA_real_,          NA_real_,
  "",              FALSE,                          FALSE,                    FALSE,                 NA_real_,          NA_real_
)

emptySampleDescr <- tribble(
  ~`Identifier 1`, ~`Identifier 2`, ~Tray, ~`Is standard?`, 
  "",              "",              1,     F,
  "",              "",              1,     F,
  "",              "",              1,     F,
  "",              "",              1,     F,
  "",              "",              1,     F
)
