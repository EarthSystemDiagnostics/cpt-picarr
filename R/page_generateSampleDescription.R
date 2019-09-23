library(shiny)
library(rhandsontable)
library(tidyverse)
library(futile.logger)

#' pageGenerateSampleDescrUI
#' 
#' UI function for the page 'Generate a sample description'
#'
#' @param id Identifier for the namespace of this module
#'
#' @return A HTML tag object
pageGenerateSampleDescrUI <- function(id){
  
  # create namespace function
  ns <- NS(id)
  
  tagList(
    h2("Generate a sample description file"), 
    textOutput(ns("projectName")), br(),
    
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
      uiOutput(ns("downloadButtonSpace")),
      textOutput(ns("helpMessageDownload"))
    ),
    
    actionButton(ns("goToPageProject"), "Back to page 'Project'"),
    
    # silently pass the given id to the server function
    conditionalPanel("false", textInput(ns("id"), label = "", value = id))
  )
}

#' pageGenerateSampleDescr
#'
#' Implements the server logic for the page 'Generate Sample Description'.
#'
#' @param input Shiny inputs
#' @param output Shiny outputs
#' @param session Shiny session
#' @param project A reactive expression. 'project()' evaluates to a 
#'                String -> the name of the currently loaded project
#' @param serverEnvironment An environment. The environment of the 
#'                          server function that calls this module.
#'                          Used to execute code in the environment of the 
#'                          main server function for the app (e.g. to 
#'                          switch between pages).
#'
#' @return No explicit return value
pageGenerateSampleDescr <- function(input, output, session, project, serverEnvironment){
  
  # -------- REACTIVE VALUES -----------------
  
  rv <- reactiveValues()
  
  # store up-to date sample description table
  rv$sampleDescr <- emptySampleDescr
  observeEvent(input$hotSampleDescr, {
    rv$sampleDescr <- hot_to_r(input$hotSampleDescr)
    flog.debug("sample description updated")
  })
  
  # store up-to date processing options table
  rv$processingOptions <- processingOptionsInitial
  observeEvent(input$hotProcessingOptions, {
    rv$processingOptions <- hot_to_r(input$hotProcessingOptions)
    flog.debug("processing options updated")
  })
  
  # Used to match measurement data with its sample description and processing template.
  # Is appended to Identifer 2 to preserve through the measurement process.
  rv$uniqueIdentifier <- NULL
  
  # create namespace function using the input id
  ns <- reactive(NS(isolate(input$id)))
  
  # --------  UPDATE TEMPLATES -----------
  
  observeEvent(project(), {
    updateTemplateSelectionListSampleDescr(session, NULL, project())
    updateTemplateSelectionListProcessing(session, NULL, project())
    flog.debug("loaded templates on page 'generate sample description' (project: %s)", project())
  })
  
  # -------- DISPLAY CURRENT PROJECT ------------
  
  output$projectName <- renderText(sprintf("Project: %s", project()))
  
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
    rv$sampleDescr <- add_row(rv$sampleDescr, tray = 1, `Is standard?` = FALSE)
  })
  
  observeEvent(input$saveNewTemplateSampleDescr, {
    
    name <- input$templateNameSampleDescr
    
    helpMessage <- saveNewTemplate(rv$sampleDescr, name, project(), "sampleDescription")
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
    rv$processingOptions <- add_row(data, `Use for drift correction` = F, 
                                    `Use for calibration` = F, `Use as control standard` = F)
  })
  
  observeEvent(input$saveNewTemplateProcessing, {
    
    name <- input$templateNameProcessing
    data <- hot_to_r(input$hotProcessingOptions)
    
    helpMessage <- saveNewTemplate(data, name, project(), "processing")
    output$helpMessageProcessing <- renderText(helpMessage)
    
    updateTemplateSelectionListProcessing(session, name, project())
  })
  
  # -------- DOWNLOAD AND SAVE ----------------
  
  output$downloadButtonSpace <- renderUI({
    flog.debug("re-rendering download button space")
    outputElement <- validateSampleDescrAndProccOptions(
      rv$sampleDescr,
      rv$processingOptions,
      ns()("download")
    )
    return(outputElement)
  })

  output$download <- downloadHandler(
    filename = function(){
      sprintf("%s_%s.csv", Sys.Date(), project())
    },
    content = function(file){
      data <- rv$sampleDescr
      rv$uniqueIdentifier <- generateUniqueIdentifer(data)
      downloadSampleDescr(data, file, rv$uniqueIdentifier)
    }
  )
  
  observeEvent(rv$uniqueIdentifier, {
    
    processingOptions <- hot_to_r(input$hotProcessingOptions)
    uniqueIdentifier <- rv$uniqueIdentifier
    
    saveOnServer(rv$sampleDescr, processingOptions, uniqueIdentifier)
    
    # display sucess message
    output$helpMessageDownload <- renderText(sprintf(
      "Sample description and processing options saved on server. Unique identifier: %s", uniqueIdentifier))
  })
  
  # ------- GO BACK TO PAGE PROJECT ----------
  
  observeEvent(input$goToPageProject, goToPage("Project", serverEnvironment))
}

######################################
# HELPERS 
######################################

#' @return A help message to be displayed next to the save button
saveNewTemplate <- function(data, name, project, mode = "sampleDescription", basePath = BASE_PATH){
  
  if (name == "") return("Please enter a name for the template")
  
  templateDir <- file.path(basePath, project, "templates", mode)
  file <- file.path(templateDir, name)
  
  flog.debug("saving new template to %s", file)
  
  # Fix bug where the user cannot change the Identifier 2 column whenever it 
  # consists only of NA values. Fix: prefix all Identifier 2 values with a 
  # length zero character.
  mutate(data, `Identifier 2` = str_c("\U200B"), `Identifier 2`)
  
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
    hot_col(col = "tray", type = "dropdown", source = 1:2)
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

generateUniqueIdentifer <- function(data){
  sampleDescrHash <- digest::digest(data)  # create an md5 hash of the sample description
  timestamp <- as.numeric(Sys.time())
  str_c(sampleDescrHash, timestamp)
}

downloadSampleDescr <- function(data, file, uniqueIdentifier){
  
  flog.info("Dowloading sample description")
  flog.debug(str_c("Unique identifier: ", uniqueIdentifier))
  
  data <- data %>%
    select(`Identifier 1`, `Identifier 2`, tray) %>%
    mutate(`Identifier 2` = str_replace_na(`Identifier 2`, replacement = "")) %>%
    mutate(`Identifier 2` = str_c(`Identifier 2`, "_", uniqueIdentifier)) %>%
    rowid_to_column("vial") %>%
    select(tray, vial, `Identifier 1`, `Identifier 2`) # set explicit column order
  
  write_csv(data, file)
}

saveOnServer <- function(sampleDescr, processingOptions, uniqueIdentifier, basePath = BASE_PATH){
  
  path <- file.path(basePath, "processingOptions", uniqueIdentifier)
  
  dir.create(path, recursive = TRUE)
  write_csv(sampleDescr, file.path(path, "sampleDescription.csv"))
  write_csv(processingOptions, file.path(path, "processingOptions.csv"))
}

validateSampleDescrAndProccOptions <- function(sampleDescription, processingOptions, id){
  
  # check if input values are null
  if (is.null(sampleDescription))
    return(p("Sample description is empty."))
  if (is.null(processingOptions))
    return(p("Processing options table is empty."))
  
  standardsInSampleDesc <- sampleDescription %>%
    filter(`Is standard?`) %>%
    .$`Identifier 1` %>%
    unique()
  
  standardsInProccOptions <- processingOptions$`Identifier 1`
  
  # check if all standards are included in the processing options
  for (std in standardsInSampleDesc)
    if (!std %in% standardsInProccOptions) 
      return(p("Input error: The standard '", std, 
               "' is missing from the processing options.", style = "color:red"))
  
  # check that no standards are in the proccessing options that are not in the sample descr
  for (std in standardsInProccOptions)
    if (!std %in% standardsInSampleDesc) 
      return(p("Input error: The standard '", std, 
               "' is included in the processing options but not in the sample description.",
               style = "color:red"))
  
  # check if processing options contain duplicates
  if (any(duplicated(processingOptions$`Identifier 1`)))
    return(p("Input error: The processing options column 'Identifier 1' contains duplicates.", 
             style = "color:red"))
  
  # check that there are no misssing values in processing options
  if (any(is.na(processingOptions)))
    return(p("Input error: The processing options table contains empty cells.", style = "color:red"))
  
  # check that control standard is not selected for anything else
  for (row in 1:nrow(processingOptions)){
    isControlStandard <- processingOptions[[row, "Use as control standard"]]
    isUsedForSomethingElse <- processingOptions[[row, "Use for calibration"]] || 
      processingOptions[[row, "Use for drift correction"]]
    isBadEntry <- isControlStandard && isUsedForSomethingElse
    if (isBadEntry) 
      return(p("Input error: The standard '", processingOptions[[row, "Identifier 1"]], 
               "' is selected as control standard. It may not be selected for anything else.",
               style = "color:red"))
  }
  
  # sample description and processing options were validated sucessfully
  return(downloadButton(id, "Download sample description", style = blue))
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
  ~`Identifier 1`, ~`Identifier 2`, ~tray, ~`Is standard?`, 
  "",              "",              1,     F,
  "",              "",              1,     F,
  "",              "",              1,     F,
  "",              "",              1,     F,
  "",              "",              1,     F
)
