library(shiny)
library(futile.logger)

# the app should run both from the root directory and from R/
if ("./R" %in% list.dirs(recursive = FALSE)){
  source("R/page_processData.R")
  source("R/page_generateSampleDescription.R")
  source("R/global.R")
} else {
  source("page_processData.R")
  source("page_generateSampleDescription.R")
  source("global.R")
}

# display all logging messages
flog.threshold(DEBUG)

ui <- navbarPage(
    "Cpt. Picarr", id = "page",
    
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
  
  callModule(pageProcessData, "processData")
  callModule(pageGenerateSampleDescr, "sampleDescription")
}

shinyApp(ui, server)