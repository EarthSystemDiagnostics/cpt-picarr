library(shiny)
library(futile.logger)
library(rlist)

# the app should run both from the root directory and from R/
if ("./R" %in% list.dirs(recursive = FALSE)){
  source("R/page_home.R")
  source("R/page_processData.R")
  source("R/page_generateSampleDescription.R")
  source("R/page_uploadData.R")
  source("R/page_project.R")
  source("R/global.R")
  source("R/helpers_processDataWithPiccr.R")
  source("R/helpers_createOrLoadProject.R")
  source("R/helpers_goToTab.R")
} else {
  source("page_home.R")
  source("page_processData.R")
  source("page_generateSampleDescription.R")
  source("page_uploadData.R")
  source("page_project.R")
  source("global.R")
  source("helpers_processDataWithPiccr.R")
  source("helpers_createOrLoadProject.R")
  source("helpers_goToTab.R")
}

# display all logging messages
flog.threshold(DEBUG)

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
    )
)

server <- function(input, output, session){
  
  # ------------ INITIALIZATION --------------
  
  rv <- reactiveValues()
  rv$project <- NULL
  
  # reactive value to pass to modules
  project <- reactive({rv$project})
  
  # ------------- CALL MODULES -------------
  
  ownEnvir <- environment()
  callModule(pageHome, "home", serverEnvironment = ownEnvir)
  callModule(pageProject, "project", project = project, serverEnvironment = ownEnvir)
  callModule(pageGenerateSampleDescr, "sampleDescription", project = project)
  callModule(pageUploadData, "uploadData", project = project)
  callModule(pageProcessData, "processData")
  
}

shinyApp(ui, server)
