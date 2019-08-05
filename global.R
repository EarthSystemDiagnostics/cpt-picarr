blue <- "color: #fff; background-color: #337ab7; border-color: #2e6da4"


df_assembly_prot_template <- data.frame(
  id_1 = letters[1:20],
  id_2 = LETTERS[1:20],
  num_injections = as.integer(3),
  is_standard = FALSE,
  standard_oxy_17 = "",
  standard_oxy_18 = "",
  standard_dtr = "",
  stringsAsFactors = FALSE
)
colnames(df_assembly_prot_template) <- c("Identifier 1", "Identifier 2", "Number of injections", "Is standard?", "True oxy-17 (only for standards)", "True oxy-18 (only for standards)", "True dtr (only for standards)")


df_processing_template <- data.frame(
  id_1 = letters[1:4],
  id_2 = letters[4:1],
  use_for_memory = TRUE,
  use_for_drift = TRUE,
  use_for_calibration = TRUE,
  use_as_control_standard = FALSE,
  stringsAsFactors = FALSE
)
colnames(df_processing_template) <- c("Identifier 1", "Identifier 2", "Use for memory correction?", "Use for drift correction?", "Use for calibration?", "Use as control standard?")