library(shiny)

if ("./R" %in% list.dirs(recursive = FALSE)){
  source("R/page_processData.R")
  source("R/global.R")
} else {
  source("page_processData.R")
  source("global.R")
}

ui <- navbarPage(
    "Cpt. Picarr", id = "page",
    
    tabPanel(
      "Process measurement data",
      pageProcessDataUI("processData")
    )
)

server <- function(input, output, session){
  
  callModule(pageProcessData, "processData")
}

shinyApp(ui, server)