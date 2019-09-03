library(shiny)
library(futile.logger)
library(rlist)

# the app should run both from the root directory and from R/
if ("./R" %in% list.dirs(recursive = FALSE)){
  source("R/page_home.R")
  source("R/page_processData.R")
  source("R/page_generateSampleDescription.R")
  source("R/page_uploadData.R")
  source("R/global.R")
  source("R/helpers_processDataWithPiccr.R")
  source("R/helpers_createOrLoadProject.R")
  source("R/helpers_goToTab.R")
} else {
  source("page_home.R")
  source("page_processData.R")
  source("page_generateSampleDescription.R")
  source("page_uploadData.R")
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
      uiOutput("header"),
      wellPanel(
        h3("Project Information"),
        p(strong("Project name:")),
        textOutput("projInfoName"), br(),
        p(strong("People involved:")),
        textOutput("projInfoPeople"), br(),
        p(strong("Additional info:")),
        textOutput("projInfoAdditional"), br(),
        p(strong("Expedition date:")),
        textOutput("projInfoDate")
      ),
      wellPanel(
        # TODO
        h3("Project Data"),
        p("TODO")
      ),
      wellPanel(
        h3("What do you want to do next?"),
        
        actionButton("goToGenerateSampleDescr", "Generate a sample description", style = blue),
        actionButton("goToUploadData", "Upload measurement data", style = blue),
        actionButton("goToProcessData", "Process measurement data", style = blue)
        
      )
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
  rv$project <- ""
  
  hideAllTabs()
  
  # ------------- CALL MODULES -------------
  
  callModule(pageProcessData, "processData")
  callModule(pageGenerateSampleDescr, "sampleDescription", project = rv$project)
  callModule(pageUploadData, "uploadData", project = rv$project)
  callModule(pageHome, "home", serverEnvironment = environment())
  
  # ------------ NAVIGATION BETWEEN PAGES -----------
  
  observeEvent(input$goToGenerateSampleDescr, goToTab("Generate a sample description", session))
  observeEvent(input$goToProcessData, goToTab("Process measurement data", session))
  observeEvent(input$goToUploadData, goToTab("Upload measurement data", session))
  
  # ------------ REACT TO USER INPUT --------------
  
  # render output on the project page
  observeEvent(rv$project, {
    
    projectName <- rv$project
    req(projectName)
    
    # display project information
    projectInfo               <- loadProjectInfo(projectName)
    output$projInfoName       <- renderText(projectInfo$name)
    output$projInfoPeople     <- renderText(projectInfo$people)
    output$projInfoAdditional <- renderText(projectInfo$additionalInfo)
    output$projInfoDate       <- renderText(projectInfo$date)
    
    # TODO: display project data
    
  })
}

######################
# HELPERS
######################

loadProjectInfo <- function(projectName, basePath = BASE_PATH){
  path <- file.path(basePath, projectName, "projectInfo.json")
  rlist::list.load(path)
}

hideAllTabs <- function(){
  hideTab("page", target = "Generate a sample description")
  hideTab("page", target = "Process measurement data")
  hideTab("page", target = "Project")
}

shinyApp(ui, server)
