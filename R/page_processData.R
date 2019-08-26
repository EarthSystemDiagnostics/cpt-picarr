source("global.R")

pageProcessDataUI <- function(id){
  
  # create namespace function
  ns <- NS(id)
  
  tagList(
    h2("Process isotope measurement data"),
    p(em("Selected project: Project A")), br(),
    wellPanel(
      h4("Setup and Options"), br(),
      selectInput(ns("data_to_process"), "Select one or more datasets to process", c("Dataset A", "Dataset B", "Dataset C"), multiple = TRUE),
      dateRangeInput(ns("data_to_process_range"), "Process all the data in this timespan (overrides the selection above):"),
      actionButton(ns("change_processing_template"), "Update the processing template for one of the datasets"), p(""), br(),
      radioButtons(ns("use_memory_correction"), "Use memory correction?", c("Yes", "No")),
      radioButtons(ns("calibration_type"), "Use three-point calibration or two-point calibration?", c("Use three-point calibration",
                                                                                               "Use two-point calibration")),
      radioButtons(ns("drift_and_calibration"), "Drift correction and calibration options", c("Use linear drift correction and calibration", 
                                                                                       "Use double calibration",
                                                                                       "Use only calibration, without drift correction")),
      h4("All set up?"), br(),
      actionButton(ns("do_process"), "Process the data", style = blue),
      actionButton(ns("do_download"), "Download the processed data"),
      actionButton(ns("do_save"), "Save the processed data on the server")
    ),
    wellPanel(
      h4("Plots"), br(),
      tabsetPanel(
        tabPanel("Dataset-level plots", br(),
                p("This section contains plots concerning memory and drift correction, and the calibration for an individual dataset."),
                selectInput(ns("dataset_for_plotting_at_dataset_level"), "Select a dataset for plotting", c("Dataset A", "Dataset B", "Dataset C")),
                h4("Memory coefficients"),
                plotOutput(ns("plot_memory_correction")),
                h4("Drift"),
                plotOutput(ns("plot_drift_correction")),
                h4("Calibration"),
                plotOutput(ns("plot_calibration")),
                h4("Raw vs. processed"),
                plotOutput(ns("plot_raw_vs_processed")),
                h4("Deviation from true value for standards"),
                tableOutput(ns("table_deviation"))
        ),
        tabPanel("Probe-level plots", br(),
                p("This section contains plots with regards to the measured values for individual probes in a specific dataset."),
                selectInput(ns("dataset_for_plotting_at_probe_level"), "Select a dataset for plotting", c("Dataset A", "Dataset B", "Dataset C")),
                plotOutput(ns("plot_stddev")),
                plotOutput(ns("plot_probes"))
        ),
        tabPanel("Summary plots", br(),
                p("This section contains plots comparing the data quality of different datasets."),
                selectInput(ns("datasets_for_plotting_summary"), "Select the datasets for plotting", c("Dataset A", "Dataset B", "Dataset C"), 
                            multiple = TRUE, selected = c("Dataset A", "Dataset B", "Dataset C")),
                plotOutput(ns("plot_summary"))
        )
      )
    )
  )
}

pageProcessData <- function(input, output, session){
  
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
  probes <- read_csv("www/probes_d180.csv")
  
  mem_corr_plot <- renderPlot({
    ggplot(mem, mapping = aes(x = InjNr, y = MemCoeff, color = Standard)) + 
      geom_point() + 
      geom_line() + 
      labs(x = "Injection Nr.", y = "memory coefficient") + 
      facet_grid(cols = vars(d)) + 
      scale_x_continuous(breaks = unique(mem$InjNr))
  })
  output$plot_memory_correction <- mem_corr_plot
  output$plot_memory_correction_qe <- mem_corr_plot
  
  drift_corr_plot <- renderPlot({
    ggplot(drift, mapping = aes(x = Block, y = Deviation, color = Standard)) + 
      geom_point() + 
      geom_line() + 
      labs(x = "Block Nr.", y = "Deviation from initial measurement") + 
      facet_grid(cols = vars(d)) + 
      scale_x_continuous(breaks = unique(drift$Block))
  })
  output$plot_drift_correction <- drift_corr_plot
  output$plot_drift_correction_qe <- drift_corr_plot
  
  calibration_plot <- renderPlot({
    ggplot(calibration, mapping = aes(x = True, y = Measured, color = Standard)) + 
      geom_point() + 
      geom_abline() + 
      facet_grid(cols = vars(d)) + 
      labs(x = "True value for standard", y = "Measured value")
  })
  output$plot_calibration <- calibration_plot
  output$plot_calibration_qe <- calibration_plot
  
  raw_v_proc_plot <- renderPlot({
    ggplot(raw_vs_processed, mapping = aes(x = `Identifier 1`, y = value, color = state)) + 
      geom_point() + 
      facet_grid(cols = vars(d)) + 
      facet_wrap(vars(d), scales = "free_x") + 
      xlab("sample Identifer 1") + 
      coord_flip()
  })
  output$plot_raw_vs_processed <- raw_v_proc_plot
  output$plot_raw_vs_processed_qe <- raw_v_proc_plot
  
  dev_table <- renderTable({
    dev
  })
  output$table_deviation <- dev_table
  output$table_deviation_qe <- dev_table
  
  
  probes_plot <- renderPlot({
    df <- probes
    plot(df$probe, df$d180_measured, type = "p", col = "blue", xlab = "probe", ylab = "d180")
    points(df$probe, df$d180_memory_corrected, col = "red")
    points(df$probe, df$d180_drift_corrected, col = "black")
    points(df$probe, df$d180_calibrated, col = "purple")
    legend("topleft", c("measured value", "memory corrected", "drift corrected", "calibrated"), 
           fill = c("blue", "red", "black", "purple"))
    grid()
  })
  output$plot_probes <- probes_plot
  output$plot_probes_qe <- probes_plot
  
  stddev_plot <- renderPlot({
    plot(1:5, c(0.001, 0.0015, 0.0009, 0.003, 0.00005), xlab = "probe", ylab = "std deviation", main = "standard deviation of each probe", col = "blue")
    grid()
  })
  output$plot_stddev <- stddev_plot
  output$plot_stddev_qe <- stddev_plot
  
  output$plot_summary <- renderPlot({
    plot(c(1, 2, 3), c(0.02, 0.015, 0.3), main = "Error of the control standard", ylab = "Error of the control standard", xlab = "dataset", xaxt = "n", col = "blue")
    axis(1, at = 1:3, labels = c("Dataset A", "Dataset B", "Dataset C"))
    grid()
  })
}