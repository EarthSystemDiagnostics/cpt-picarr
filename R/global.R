blue <- "color: #fff; background-color: #337ab7; border-color: #2e6da4"


df_best_prot_template <- data.frame(
  id_1 = letters[1:20],
  id_2 = LETTERS[1:20],
  num_injections = as.integer(3),
  is_standard = FALSE,
  standard_true_value = "",
  stringsAsFactors = FALSE
)
colnames(df_best_prot_template) <- c("Identifier 1", "Identifier 2", "Number of injections", "Is standard?", "True isotope concentration (only for standards)")


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