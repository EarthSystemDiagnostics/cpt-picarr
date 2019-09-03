library(shiny)
library(futile.logger)
library(rlist)

# the app should run both from the root directory and from R/
if ("./R" %in% list.dirs(recursive = FALSE)){
  source("R/page_processData.R")
  source("R/page_generateSampleDescription.R")
  source("R/global.R")
  source("R/helpers_processDataWithPiccr.R")
  source("R/helpers_createOrLoadProject.R")
} else {
  source("page_processData.R")
  source("page_generateSampleDescription.R")
  source("global.R")
  source("helpers_processDataWithPiccr.R")
  source("helpers_createOrLoadProject.R")
}

# display all logging messages
flog.threshold(DEBUG)

ui <- navbarPage(
    "Cpt. Picarr", id = "page",
    
    tabPanel(
      "Home",
      h2("Welcome to Cpt. Picarr!"),
      h4("What do you want to do today?"),
      
      wellPanel(
        h3("Load an existing project"),
        p("See information and data for an existing project. Next you can generate 
            a sample description, upload measurement data, or process measurement data."),
        selectInput("projectToLoad", "Choose a project", c()),
        actionButton("loadProject", "Load selected project", style = blue)
      ),
      
      wellPanel(
        h3("Create a new project"),
        p("Create a new projecte to manage related data."),
        textInput("projectName", "Project name"),
        textInput("projectPeople", "People involved (optional)"),
        textAreaInput("projectAdditionalInfo", "Additional info (optional)"),
        dateInput("projectDate", "Expedition date (optional)", value = NA),
        actionButton("createProject", "Create new project", style = blue),
        textOutput("infoMessage")
      )
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
    )
)

server <- function(input, output, session){
  
  rv <- reactiveValues()
  rv$project <- NULL  # the loaded project
  
  updateProjectSelection(session)
  hideAllTabs()
  
  observeEvent(input$loadProject, {
    goToTab("Project", session)
    rv$project <- input$projectToLoad
  })
  observeEvent(input$goToGenerateSampleDescr, goToTab("Generate a sample description", session))
  observeEvent(input$goToProcessData, goToTab("Process measurement data", session))
  observeEvent(input$goToUploadData, goToTab("Upload measurement data", session))
  
  observeEvent(input$createProject, {
    
    name <- input$projectName
    req(name)
    
    if (projectExistsAlready(name)) {
      output$infoMessage <- renderText(
        sprintf("Project named '%s' exists already. Please choose a different name.", name))
    } else {
      createProjectDirectory(name)
      createProjectInfoFile(input)
      updateProjectSelection(session)
      output$infoMessage <- renderText("New project created.")
      goToTab("Project", session)
      rv$project <- name
    }
  })
  
  observeEvent(rv$project, {
    
    projectName <- rv$project
    
    # display project information
    projectInfo               <- loadProjectInfo(projectName)
    output$projInfoName       <- renderText(projectInfo$name)
    output$projInfoPeople     <- renderText(projectInfo$people)
    output$projInfoAdditional <- renderText(projectInfo$additionalInfo)
    output$projInfoDate       <- renderText(projectInfo$date)
    
    # display project data
    
  })
  
  callModule(pageProcessData, "processData")
  callModule(pageGenerateSampleDescr, "sampleDescription", project = rv$project)
}

loadProjectInfo <- function(projectName, basePath = BASE_PATH){
  path <- file.path(basePath, projectName, "projectInfo.json")
  rlist::list.load(path)
}

hideAllTabs <- function(){
  hideTab("page", target = "Generate a sample description")
  hideTab("page", target = "Process measurement data")
  hideTab("page", target = "Project")
}

goToTab <- function(target, session) {
  hideAllTabs()
  showTab("page", target = target)
  updateTabsetPanel(session, "page", selected = target)
}


shinyApp(ui, server)