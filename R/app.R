library(shiny)
library(rhandsontable)

source("global.R")

ui <- fluidPage(
  navlistPanel(
    "Welcome to Cpt. Picarr!",
    
    tabPanel("Home",
             h3("Welcome to Cpt. Picarr!"), 
             p("Start by loading an existing project or creating a new one. You can also look at cross-project 
               statistics to analyze the performance of your measurement instruments over time."), br(),
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
             ),
             wellPanel(
               h3("Take a look at cross-project statistics"),
               actionButton("go_to_cross_project_statistics", "Go to page 'Instrument performance'", style = blue)
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
                 dateInput("measurement_date", "When was this data measured?"),
                 textAreaInput("file_addtional_info", "Information about the dataset (optional)")
               ),
               tabPanel(
                 "Templates", br(),
                 selectInput("template_for_file_upload", "Select a Bestückungsprotokoll template to associate with the dataset", c("Template A", "Template B")),
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
                          plotOutput("plot_memory_correction"),
                          plotOutput("plot_drift_correction"),
                          p("TODO: plot calibration. What should the calibration plot look like?")
                 ),
                 tabPanel("Probe-level plots", br(),
                          p("This section contains plots with regards to the measured values for individual probes in a specific dataset."),
                          selectInput("dataset_for_plotting_at_probe_level", "Select a dataset for plotting", c("Dataset A", "Dataset B", "Dataset C")),
                          plotOutput("plot_stddev"),
                          plotOutput("plot_probes")
                 ),
                 tabPanel("Summary plots", br(),
                          p("This section contains plots comparing the data quality of different datasets."),
                          selectInput("datasets_for_plotting_summary", "Select the datasets for plotting", c("Dataset A", "Dataset B", "Dataset C"), 
                                      multiple = TRUE, selected = c("Dataset A", "Dataset B", "Dataset C")),
                          plotOutput("plot_summary"),
                          p("TODO: What other plots would you like to have?")
                 )
               )
             )
    ),
    
    tabPanel("Instrument performance",
             h3("TODO")
    )
  )
)

server <- function(input, output){
  output$ho_table_best_prot <- renderRHandsontable(rhandsontable(df_best_prot_template))
  output$ho_table_processing <- renderRHandsontable(rhandsontable(df_processing_template))
  
  output$plot_memory_correction <- renderPlot(plot(read.csv("../www/memory_correction.csv", header = FALSE), main = "memory correction", 
                                                   ylab = "memory coefficient", xlab = "number of injections", type = "b", col = "blue"))
  output$plot_drift_correction <- renderPlot(plot(read.csv("../www/drift_correction.csv", header = FALSE), main = "drift correction", 
                                                  ylab = "deviation from initial measurement", xlab = "standard block", type = "b", col = "blue"))

  
  output$plot_probes <- renderPlot({plot(df$probe, df$d180_measured, type = "p", col = "blue", xlab = "probe", ylab = "d180")
                                    points(df$probe, df$d180_memory_corrected, col = "red")
                                    points(df$probe, df$d180_drift_corrected, col = "black")
                                    points(df$probe, df$d180_calibrated, col = "purple")
                                    legend("topleft", c("measured value", "memory corrected", "drift corrected", "calibrated"), fill = c("blue", "red", "black", "purple"))
                                    grid()})
  output$plot_stddev <- renderPlot({plot(1:5, c(0.001, 0.0015, 0.0009, 0.003, 0.00005), xlab = "probe", ylab = "std deviation", main = "standard deviation of each probe", col = "blue")
                                    grid()})
  
  output$plot_summary <- renderPlot({plot(c(1, 2, 3), c(0.02, 0.015, 0.3), main = "Error of the control standard", 
                                          ylab = "Error of the control standard", xlab = "dataset", xaxt = "n", col = "blue")
                                     axis(1, at = 1:3, labels = c("Dataset A", "Dataset B", "Dataset C"))
                                     grid()})
}

shinyApp(ui, server)