library(shiny)
library(rhandsontable)


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
    
    tabPanel("Generate a Bestückungsprotokoll",
             wellPanel(
               h3("Select a template to use"),
               selectInput("template", "", c("Template A", "Template B")),
               actionButton("load_template", "Load selected template")
             ),
             rHandsontableOutput("ho_table"), br(),
             wellPanel(
               h3("All done?"),
               actionButton("new_template", "Save as new template"),
               actionButton("generate_best_protocol", "Generate Bestückungsprotokoll", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
             )
    ),
             
    tabPanel("Upload measurement data", "contents"),
    tabPanel("Process measurement data", "contents")
  )
)

server <- function(input, output){
  output$ho_table <- renderRHandsontable(rhandsontable(
    data.frame(
      id_1 = letters[1:20],
      id_2 = LETTERS[1:20],
      num_injections = as.integer(3),
      is_standard = FALSE,
      stringsAsFactors = FALSE
    )
  ))
}

shinyApp(ui, server)