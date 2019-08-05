library(shiny)

ui <- fluidPage(
  navlistPanel(
    "Welcome to Cpt. Picarr!",
    tabPanel("Select a project",
            wellPanel(
             h3("Load an existing project"),
             selectInput("project_to_load", "Choose a project", c("Project A", "Project B", "Project C")),
             actionButton("load_project", "Load selected project")
            ),
            wellPanel(
             h3("Create a new project"),
             textInput("proj_name", "Project name"),
             textInput("proj_people", "People involved"),
             textInput("proj_additional_info", "Additional info"),
             dateInput("proj_date", "Expedition date"),
             actionButton("create_project", "Create new project")
            )
    ),
    tabPanel("Generate a Bestückungsprotokoll", "contents"),
    tabPanel("Upload measurement data", "contents"),
    tabPanel("Process measurement data", "contents")
  )
)

server <- function(input, output){}

shinyApp(ui, server)