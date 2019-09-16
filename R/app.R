library(shiny)
library(futile.logger)
library(rlist)
library(tidyverse)

# ----------------- SOURCE SCRIPTS ----------------------
# Source all the files in the directory "R" that end in ".R" and 
# are not "app.R". Should run from "R" and from the repository
# root directory.

codeDirectory <- if(dir.exists("R")) "R" else "."
files <- list.files(codeDirectory, pattern = "*.R", full.names = TRUE)
filesToSource <- files[!endsWith(files, "app.R")]
walk(filesToSource, source)

# ------------------ SET GLOBAL VARIABLES AND OPTIONS -----------

# display all logging messages
flog.threshold(DEBUG)

# Use blue as style attribute to make blue buttons. Used by the modules.
# Set in the global environment to avoid namespacing issues when
# the modules try to access the variable.
# Example usage: actionButton("id", "label", style = blue)
assign(
  "blue", 
  "color: #fff; background-color: #337ab7; border-color: #2e6da4", 
  envir = globalenv()
)

# Set the BASE_PATH
#
# The BASE_PATH is the path to the directory that contains all the
# application data. It is set in the file 'config.yaml'.
# Note that the working directory may be "R" or the repository root
# directory. Therefore the config path has to be determined dynamically.
configPath <- if(file.exists("config.yaml")) "config.yaml" else file.path("..", "config.yaml")
BASE_PATH <- list.load(configPath)$BASE_PATH
# set BASE_PATH in global environemnt to prevent namespacing issues
assign("BASE_PATH", BASE_PATH, envir = globalenv())

# ------------------ INITIALIZE DIRECTORY STRUCTURE ---------------
# This is most useful when the app is run for the first time.

dir.create(file.path(BASE_PATH, "processingOptions"), recursive = TRUE, showWarnings = FALSE)

# ------------------ UI AND SERVER DEFINITIONS FOR THE APP -------------

#' ui
#' 
#' The main UI definition for Cpt-Picarr. It relies on submodules to
#' implement the actual UI logic.
#' 
ui <- navbarPage(
    "Cpt. Picarr", id = "app",
    
    tabPanel(
      "Home",
      pageHomeUI("home")
    ),
    tabPanel(
      "Project",
      pageProjectUI("project")
    ),
    tabPanel(
      "Generate a sample description",
      pageGenerateSampleDescrUI("sampleDescription")
    ),
    tabPanel(
      "Process measurement data",
      pageProcessDataUI("processData")
    ),
    tabPanel(
      "Upload measurement data",
      pageUploadDataUI("uploadData")
    ),
    tabPanel(
      "Instrument performance",
      pageInstrumentPerformanceUI("instPerf")
    )
)

#' server
#' 
#' The main server function for Cpt-Picarr. It relies on submodules
#' to implement the actual server logic. 
#' 
#' Passes a reference to its environment to the submodules in order
#' to realize shared state and to make it possible to execute code
#' at an app-wide level. Note that submodules manipulate variables 
#' in this function's environment.
#'
#' @param input Shiny inputs
#' @param output Shiny outputs
#' @param session Shiny session
#'
#' @return No explicit return value
server <- function(input, output, session){
  
  # ------------ INITIALIZATION --------------
  
  # header should only display 'Home' at the start
  goToPage("Home", environment())
  
  rv <- reactiveValues()
  # Reactive value that should be set by the submodules in order to
  # realize shared state between those modules.
  rv$project <- NULL
  rv$projectDataChanged <- NULL
  # reactive expression to pass to modules
  project <- reactive({rv$project})
  projectDataChanged <- reactive({rv$projectDataChanged})
  
  # ------------- CALL MODULES -------------
  
  ownEnvir <- environment()
  callModule(
    pageHome, "home", 
    serverEnvironment = ownEnvir
  )
  callModule(
    pageProject, "project", 
    project = project, 
    serverEnvironment = ownEnvir, 
    projectDataChanged = projectDataChanged
  )
  callModule(
    pageGenerateSampleDescr, "sampleDescription", 
    project = project, 
    serverEnvironment = ownEnvir
  )
  callModule(
    pageUploadData, "uploadData", 
    project = project, 
    serverEnvironment = ownEnvir
  )
  callModule(
    pageProcessData, "processData", 
    project = project, 
    serverEnvironment = ownEnvir, 
    projectDataChanged = projectDataChanged
  )
  callModule(
    pageInstrumentPerformance, "instPerf",
    serverEnvironment = ownEnvir
  )
}

shinyApp(ui, server)
