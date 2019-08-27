library(shiny)
library(rhandsontable)
library(tidyverse)

pageGenerateSampleDescrUI <- function(id){
  
  # create namespace function
  ns <- NS(id)
  
  tagList(
    h2("Generate a sample description file"), br(),
    
    wellPanel(
      h3("Select a sample description template to use"),
      selectInput(ns("selectTemplate"), "", c()), p(""), br(),
      rHandsontableOutput(ns("hotSampleDescr")), br(),
      actionButton(ns("addRow"), "+ Add a row"), "(Right click table for more options)", p(""), 
      hr(),
      h4("Save as new template"),
      textInput(ns("templateName"), "Name the template"),
      actionButton(ns("saveNewTemplate"), "Save as new template"),
      textOutput(ns("helpMessage"), inline = TRUE)
    ),
    
    wellPanel(
      h3("Which standard should be used for what?"),
      rHandsontableOutput(ns("hotProcessingOptions"))
    ),
    
    wellPanel(
      h3("All done?"),
      actionButton(ns("downloadSampleDescr"), "Download sample description", style = blue)
    )
  )
}

pageGenerateSampleDescr <- function(input, output, session){
  
  updateTemplateSelectionList(session, NULL)
  
  rv <- reactiveValues()
  rv$sampleDescr <- emptySampleDescr
  
  output$hotProcessingOptions <- renderRHandsontable(rhandsontable(processingOptions))
  
  observeEvent(input$selectTemplate, {
    templateName <- input$selectTemplate
    
    if (templateName %in% c("", "Empty sample description")){
      template <- emptySampleDescr
    } else {
      file <- file.path(BASE_PATH, "templates", templateName)
      template <- read_csv(file)
    }
    
    rv$sampleDescr <- template
  })
  
  output$hotSampleDescr <- renderRHandsontable({
    rhandsontable(rv$sampleDescr) %>% 
      hot_col(col = "Is standard?", type = "checkbox") %>%
      hot_col(col = "Tray", type = "dropdown", source = 1:2) %>%
      hot_col(col = "True delta O18 (only for standards)", type = "numeric") %>%
      hot_col(col = "True delta H2 (only for standards)", type = "numeric")
  })
  
  observeEvent(input$addRow, {
    rv$sampleDescr <- add_row(rv$sampleDescr, `Tray` = 1)
  })
  
  observeEvent(input$saveNewTemplate, {
    
    if (input$templateName == "") {
      output$helpMessage <- renderText("Please enter a name for the template")
    }
    req(input$templateName)
    
    name <- input$templateName
    file <- file.path(BASE_PATH, "templates", name)
    data <- hot_to_r(input$hotSampleDescr)
    
    write_csv(data, file)
    
    output$helpMessage <- renderText("Template successfully saved.")
    
    updateTemplateSelectionList(session, name)
  })
}

#########################
# ARTEFACTS for this page
#########################

updateTemplateSelectionList <- function(session, selected){
  templates <- list.files(file.path(BASE_PATH, "templates"))
  updateSelectInput(session, "selectTemplate", 
                    choices = c("Empty sample description", templates),
                    selected = selected)
}

processingOptions <- tribble(
  ~`Identifier 1`, ~`Use for drift correction`, ~`Use for calibration`, ~`Use as control standard`,
  # ------------ / -------------------------- / --------------------- / -------------------------
  "",              FALSE,                          FALSE,                    FALSE,
  "",              FALSE,                          FALSE,                    FALSE,
  "",              FALSE,                          FALSE,                    FALSE,
  "",              FALSE,                          FALSE,                    FALSE,
  "",              FALSE,                          FALSE,                    FALSE
)

emptySampleDescr <- tribble(
  ~`Identifier 1`, ~`Identifier 2`, ~Tray, ~`Is standard?`, 
  ~`True delta O18 (only for standards)`, ~`True delta H2 (only for standards)`,
  "", "", 1, F, "", "",
  "", "", 1, F, "", "",
  "", "", 1, F, "", "",
  "", "", 1, F, "", "",
  "", "", 1, F, "", ""
)
