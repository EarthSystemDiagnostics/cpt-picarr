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
             textInput("proj_people", "People involved (optional)"),
             textAreaInput("proj_additional_info", "Additional info (optional)"),
             dateInput("proj_date", "Expedition date (optional)"),
             actionButton("create_project", "Create new project")
            )
    ),
    
    tabPanel("Generate a Bestückungsprotokoll",
             wellPanel(
               h3("Select a template to use"),
               selectInput("template_for_bst_prot", "", c("Template A", "Template B")),
               actionButton("load_template", "Load selected template")
             ),
             rHandsontableOutput("ho_table"), br(),
             wellPanel(
               h3("All done?"),
               actionButton("new_template", "Save as new template"),
               actionButton("generate_best_protocol", "Generate Bestückungsprotokoll", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
             )
    ),
             
    tabPanel("Upload measurement data",
             h3("Upload a file with isotope measurement data"), br(),
             fileInput("input_file", "Select a file to upload"),
             textInput("dataset_name", "Name the dataset"),
             selectInput("template_for_file_upload", "Select a Bestückungsprotokoll template", c("Template A", "Template B")),
             p("TODO: Auswertung Template (auswählen und/oder anlegen)"),
             textAreaInput("file_addtional_info", "Information about the file (optional)"), br(),
             actionButton("upload_file", "Upload the selected file")
    ),
    
    tabPanel("Process measurement data",
             h3("Post-process isotope measurement data"), br(),
             wellPanel(
               h4("Setup and Options"), br(),
               selectInput("data_to_process", "Select one or more datasets to process", c("Dataset A", "Dataset B", "Dataset C"), multiple = TRUE),
               radioButtons("use_memory_correction", "Use memory correction?", c("Yes", "No")),
               radioButtons("drift_and_calibration", "Drift correction and calibration options", c("Use drift correction and three-point calibration", 
                                                                                                   "Use Double three-point calibration", 
                                                                                                   "[Please let me know what other options you would like to see]"))
             )
    )
    
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