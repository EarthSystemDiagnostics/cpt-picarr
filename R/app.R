library(shiny)
library(rhandsontable)
library(ggplot2)
library(readr)

source("global.R")

ui <- navbarPage(
    "Cpt. Picarr", id = "page",
    
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
               textInput("proj_lead", "Project lead(s) (optional)"),
               textAreaInput("proj_additional_info", "Additional info (optional)"),
               dateInput("proj_date", "Expedition date (optional)"),
               actionButton("create_project", "Create new project", style = blue)
             ),
             wellPanel(
               h3("Take a look at cross-project statistics"),
               actionButton("go_to_cross_project_statistics", "Go to page 'Instrument performance'", style = blue)
             )
    ),
    
    tabPanel("Project",
             h3("Project A"),
             wellPanel(
               h3("Project information"),
               p(strong("Project name: "), "Project A"),
               p(strong("Project lead(s): "), "Max Mustermann, Mike Musterfrau"),
               p(strong("Additional info: "), "This is a demo project. Most of the controls don't work and all the values are hard-coded."),
               p(strong("Expedition date: "), "2019-08-01"),
               actionButton("change_project_info", "Update the project information")
             ),
             wellPanel(
               h3("Project data"),
               p(strong("raw measurement data:"), br(), "Dataset A (dataset_a.csv, created: 2019-08-01)", br(), "Dataset B (dataset_b.csv, created: 2019-08-02)"),
               p(strong("processed measurement data:"), br(), "processed dataset A (processed_dataset_a.csv, processed: 2019-08-05)", br(), 
                        "processed dataset B (processed_dataset_b.csv, processed: 2019-08-02)"),
               actionButton("download_proj_file", "Download one or multiple datasets (as csv)"),
               actionButton("download_proj_data", "Download all project data (as zip archive)")
             ),
             wellPanel(
               h3("What do you want to do next?"),
               actionButton("go_assembly", "Generate an assembly protocol", style = blue),
               actionButton("go_upload", "Upload measurement data", style = blue),
               actionButton("go_process", "Process measurement data", style = blue)
             )
    ),
    
    tabPanel("Generate an assembly protocol",
             h3("Generate an assembly protocol"), 
             p(em("Selected project: Project A")), br(),
             wellPanel(
               h4("Select an assembly protocol template to use"),
               selectInput("template_for_bst_prot", "", c("Template A", "Template B")),
               actionButton("load_template", "Load selected template"), p(""), br(),
               rHandsontableOutput("ho_table_assembly_prot"), br(),
               actionButton("add_row", "+ Add a row"), "(Right click table for more options)", p(""), 
               actionButton("new_assp_template", "Save as new template")
             ),
             wellPanel(
               h4("Select a template for the processing of data measured with this assembly protocol"),
               selectInput("processing_template", "", c("Processing Template 1", "Processing Template 2", "Empty Template")),
               rHandsontableOutput("ho_table_processing"), br(),
               actionButton("save_processing_template", "Save as new processing template")
             ),
             wellPanel(
               h4("All done?"),
               actionButton("download_assembly_protocol", "Download assembly protocol", style = blue)
             ),
             actionButton("back_to_proj_1", "Go back to the project page")
    ),
             
    tabPanel("Upload measurement data",
             h3("Upload a file with isotope measurement data"),
             p(em("Selected project: Project A")), br(),
             wellPanel(
                 fileInput("input_file", "Select a file to upload"),
                 textInput("dataset_name", "Name the dataset"),
                 textInput("device_name", "What device was this data measured with?"),
                 dateInput("measurement_date", "When was this data measured?"),
                 textAreaInput("file_addtional_info", "Information about the dataset (optional)"),
                 selectInput("template_for_file_upload", "Select an assembly protocol to associate with the dataset", c("", "Protocol A", "Protocol B"))
               ),
             actionButton("upload_file", "Upload the dataset", style = blue),
             p(""), br(),
             actionButton("back_to_proj_2", "Go back to the project page")
    ),
    
    tabPanel("Process measurement data",
             h3("Post-process isotope measurement data"),
             p(em("Selected project: Project A")), br(),
             wellPanel(
               h4("Setup and Options"), br(),
               selectInput("data_to_process", "Select one or more datasets to process", c("Dataset A", "Dataset B", "Dataset C"), multiple = TRUE),
               dateRangeInput("data_to_process_range", "Process all the data in this timespan (overrides the selection above):"),
               actionButton("change_processing_template", "Update the processing template for one of the datasets"), p(""), br(),
               radioButtons("use_memory_correction", "Use memory correction?", c("Yes", "No")),
               radioButtons("calibration_type", "Use three-point calibration or two-point calibration?", c("Use three-point calibration",
                                                                                                            "Use two-point calibration")),
               radioButtons("drift_and_calibration", "Drift correction and calibration options", c("Use linear drift correction and calibration", 
                                                                                                   "Use double calibration",
                                                                                                   "Use only calibration, without drift correction")),
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
                          h4("Memory coefficients"),
                          plotOutput("plot_memory_correction"),
                          h4("Drift"),
                          plotOutput("plot_drift_correction"),
                          h4("Calibration"),
                          plotOutput("plot_calibration"),
                          h4("Raw vs. processed"),
                          plotOutput("plot_raw_vs_processed"),
                          h4("Deviation from true value for standards"),
                          tableOutput("table_deviation")
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
                          plotOutput("plot_summary")
                 )
               )
             ),
             actionButton("back_to_proj_3", "Go back to the project page")
    ),
    
    tabPanel("Instrument performance",
             h3("Instrument performance"),
             p("Take a look at cross-project statistics to analyze instrument performance."), br(),
             wellPanel(
               selectInput("device", "Select a measurement instrument", c("All instruments", "Device A", "Device B")),
               dateRangeInput("timespan", "Select a timespan to look at"),
               actionButton("show_instrument_stats", "Show me stats for the selected device and timespan", style = blue)
             ),
             plotOutput("instrument_error")
    )
)

server <- function(input, output, session){
  
  hide_all_tabs()
  
  observeEvent(input$go_to_cross_project_statistics, {go_to_tab("Instrument performance", session)})
  
  observeEvent(input$load_project, {go_to_tab("Project", session)})
  observeEvent(input$create_project, {go_to_tab("Project", session)})
  observeEvent(input$back_to_proj_1, {go_to_tab("Project", session)})
  observeEvent(input$back_to_proj_2, {go_to_tab("Project", session)})
  observeEvent(input$back_to_proj_3, {go_to_tab("Project", session)})
  
  observeEvent(input$go_assembly, {go_to_tab("Generate an assembly protocol", session)})
  observeEvent(input$go_upload, {go_to_tab("Upload measurement data", session)})
  observeEvent(input$go_process, {go_to_tab("Process measurement data", session)})
  
  output$ho_table_assembly_prot <- renderRHandsontable(rhandsontable(df_assembly_prot_template))
  output$ho_table_processing <- renderRHandsontable(rhandsontable(df_processing_template))
  
  
  # 
  # ------- PLOTS -----------
  #
  
  mem <- read_csv("www/memory_correction.csv")
  drift <- read_csv("www/drift_correction.csv")
  calibration <- read_csv("www/calibration.csv")
  raw_vs_processed <- read_csv("www/raw_vs_processed.csv")
  dev <- read_csv("www/sollwert_abweichung.csv")
  
  output$plot_memory_correction <- renderPlot({
    ggplot(mem, mapping = aes(x = InjNr, y = MemCoeff, color = Standard)) + 
      geom_point() + 
      geom_line() + 
      labs(x = "Injection Nr.", y = "memory coefficient") + 
      facet_grid(cols = vars(d)) + 
      scale_x_continuous(breaks = unique(mem$InjNr))
  })
  output$plot_drift_correction <- renderPlot({
    ggplot(drift, mapping = aes(x = Block, y = Deviation, color = Standard)) + 
      geom_point() + 
      geom_line() + 
      labs(x = "Block Nr.", y = "Deviation from initial measurement") + 
      facet_grid(cols = vars(d)) + 
      scale_x_continuous(breaks = unique(drift$Block))
  })
  output$plot_calibration <- renderPlot({
    ggplot(calibration, mapping = aes(x = True, y = Measured, color = Standard)) + 
      geom_point() + 
      geom_abline() + 
      facet_grid(cols = vars(d)) + 
      labs(x = "True value for standard", y = "Measured value")
  })
  output$plot_raw_vs_processed <- renderPlot({
    ggplot(raw_vs_processed, mapping = aes(x = `Identifier 1`, y = value, color = state)) + 
      geom_point() + 
      facet_grid(cols = vars(d)) + 
      facet_wrap(vars(d), scales = "free_x") + 
      xlab("sample Identifer 1") + 
      coord_flip()
  })
  output$table_deviation <- renderTable({
    dev
  })
  
  
  output$plot_probes <- renderPlot({
    plot(df$probe, df$d180_measured, type = "p", col = "blue", xlab = "probe", ylab = "d180")
    points(df$probe, df$d180_memory_corrected, col = "red")
    points(df$probe, df$d180_drift_corrected, col = "black")
    points(df$probe, df$d180_calibrated, col = "purple")
    legend("topleft", c("measured value", "memory corrected", "drift corrected", "calibrated"), fill = c("blue", "red", "black", "purple"))
    grid()
  })
  output$plot_stddev <- renderPlot({
    plot(1:5, c(0.001, 0.0015, 0.0009, 0.003, 0.00005), xlab = "probe", ylab = "std deviation", main = "standard deviation of each probe", col = "blue")
    grid()
  })
  
  output$plot_summary <- renderPlot({
    plot(c(1, 2, 3), c(0.02, 0.015, 0.3), main = "Error of the control standard", ylab = "Error of the control standard", xlab = "dataset", xaxt = "n", col = "blue")
    axis(1, at = 1:3, labels = c("Dataset A", "Dataset B", "Dataset C"))
    grid()
  })
  
  output$instrument_error <- renderPlot({
    plot(1:4, c(0.1,0.12,0.15,0.18), main = "Error of the control standard over time", ylab = "Error of control standard", xlab = "time", xaxt = "n", col = "blue", type = "b")
    axis(1, at = 1:4, labels = c("1.8.19", "2.8.19", "3.8.19", "4.8.19"))
    grid()
  })
}

hide_all_tabs <- function(){
  hideTab("page", target = "Project")
  hideTab("page", target = "Generate an assembly protocol")
  hideTab("page", target = "Upload measurement data")
  hideTab("page", target = "Process measurement data")
  hideTab("page", target = "Instrument performance")
}
go_to_tab <- function(target, session) {
  hide_all_tabs()
  showTab("page", target = target)
  updateTabsetPanel(session, "page", selected = target)
}

shinyApp(ui, server)