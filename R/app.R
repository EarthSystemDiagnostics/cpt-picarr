library(shiny)
library(rhandsontable)

blue <- "color: #fff; background-color: #337ab7; border-color: #2e6da4"

ui <- fluidPage(
  navlistPanel(
    "Welcome to Cpt. Picarr!",
    
    tabPanel("Select a project",
             h3("Welcome to Cpt. Picarr!"), 
             p("Start by loading an existing project or create a new one."), br(),
             wellPanel(
               h3("Load an existing project"),
               selectInput("project_to_load", "Choose a project", c("Project A", "Project B", "Project C")),
               actionButton("load_project", "Load selected project", style = blue)
             ),
             wellPanel(
               h3("Create a new project"),
               textInput("proj_name", "Project name"),
               textInput("proj_people", "People involved (optional)"),
               textAreaInput("proj_additional_info", "Additional info (optional)"),
               dateInput("proj_date", "Expedition date (optional)"),
               actionButton("create_project", "Create new project", style = blue)
             )
    ),
    
    tabPanel("Generate a Bestückungsprotokoll",
             wellPanel(
               h3("Select a template to use"),
               selectInput("template_for_bst_prot", "", c("Template A", "Template B")),
               actionButton("load_template", "Load selected template")
             ),
             rHandsontableOutput("ho_table_best_prot"), br(),
             wellPanel(
               h3("All done?"),
               actionButton("new_template", "Save as new template"),
               actionButton("generate_best_protocol", "Generate Bestückungsprotokoll", style = blue)
             )
    ),
             
    tabPanel("Upload measurement data",
             h3("Upload a file with isotope measurement data"), br(),
             wellPanel(tabsetPanel(
               tabPanel(
                 "Input file", br(),
                 fileInput("input_file", "Select a file to upload"),
                 textInput("dataset_name", "Name the dataset"),
                 textAreaInput("file_addtional_info", "Information about the dataset (optional)")
               ),
               tabPanel(
                 "Templates", br(),
                 selectInput("template_for_file_upload", "Select a Bestückungsprotokoll template", c("Template A", "Template B")),
                 selectInput("processing_template", "Select a template for the post-processing of this dataset", c("Template 1", "Template 2", "+ create new template")),
                 rHandsontableOutput("ho_table_processing"), br(),
                 actionButton("save_processing_template", "Save as new processing template")
               )
             )),
             actionButton("upload_file", "Upload the dataset", style = blue)
    ),
    
    tabPanel("Process measurement data",
             h3("Post-process isotope measurement data"), br(),
             wellPanel(
               h4("Setup and Options"), br(),
               selectInput("data_to_process", "Select one or more datasets to process", c("Dataset A", "Dataset B", "Dataset C"), multiple = TRUE),
               radioButtons("use_memory_correction", "Use memory correction?", c("Yes", "No")),
               radioButtons("drift_and_calibration", "Drift correction and calibration options", c("Use drift correction and three-point calibration", 
                                                                                                   "Use Double three-point calibration", 
                                                                                                   "[Please let me know what options you would like to have]")), br(),
               h4("All set up?"), br(),
               actionButton("do_process", "Process the data", style = blue),
               actionButton("do_download", "Download the processed data"),
               actionButton("do_save", "Save the processed data on the server")
             ),
             wellPanel(
               h4("Plots"), br(),
               tabsetPanel(
                 tabPanel("Dataset-level plots", br(),
                          p("This section contains plots concerning memory and drift correction, and the calibration for an individual dataset."),
                          selectInput("dataset_for_plotting_at_dataset_level", "Select a dataset for plotting", c("Dataset A", "Dataset B", "Dataset C")),
                          p("TODO: memory correction plot, drift correction plot, calibration plot.")
                 ),
                 tabPanel("Probe-level plots", br(),
                          p("This section contains plots with regards to the measured values for individual probes in a specific dataset."),
                          selectInput("dataset_for_plotting_at_probe_level", "Select a dataset for plotting", c("Dataset A", "Dataset B", "Dataset C")),
                          p("TODO: Plot the stddeviation, and the value after each processing step for each probe.")
                 ),
                 tabPanel("Summary plots", br(),
                          p("This section contains plots comparing the data quality of different datasets."),
                          selectInput("datasets_for_plotting_summary", "Select the datasets for plotting", c("Dataset A", "Dataset B", "Dataset C"), 
                                      multiple = TRUE, selected = c("Dataset A", "Dataset B", "Dataset C")),
                          p("TODO: Plot the error of the control standard")
                 )
               )
             )
    )
    
  )
)

server <- function(input, output){
  
  df_best_prot_template <- data.frame(
    id_1 = letters[1:20],
    id_2 = LETTERS[1:20],
    num_injections = as.integer(3),
    is_standard = FALSE,
    standard_true_value = "",
    stringsAsFactors = FALSE
  )
  colnames(df_best_prot_template) <- c("Identifier 1", "Identifier 2", "Number of injections", "Is standard?", "True isotope concentration (only for standards)")
  output$ho_table_best_prot <- renderRHandsontable(rhandsontable(df_best_prot_template))
  
  df_processing_template <- data.frame(
    id_1 = letters[1:4],
    id_2 = letters[4:1],
    # true_16 = 1:4,
    # true_17 = 1:4,
    # true_18 = 1:4,
    use_for_memory = TRUE,
    use_for_drift = TRUE,
    use_for_calibration = TRUE,
    use_as_control_standard = FALSE,
    stringsAsFactors = FALSE
  )
  colnames(df_processing_template) <- c("Identifier 1", "Identifier 2", # "True O-16 value", "True O-17 value", "True O-18 value", 
                                        "Use for memory correction?", "Use for drift correction?", "Use for calibration?", "Use as control standard?")
  output$ho_table_processing <- renderRHandsontable(rhandsontable(df_processing_template))
}

shinyApp(ui, server)