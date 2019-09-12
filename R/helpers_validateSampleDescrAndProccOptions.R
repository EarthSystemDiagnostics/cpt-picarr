library(shiny)
library(tidyverse)

validateSampleDescrAndProccOptions <- function(sampleDescription, processingOptions, id){
  
  # check if input values are null
  if (is.null(sampleDescription))
    return(p("Sample description is empty."))
  if (is.null(processingOptions))
    return(p("Processing options table is empty."))
  
  standardsInSampleDesc <- sampleDescription %>%
    filter(`Is standard?`) %>%
    .$`Identifier 1` %>%
    unique()
  
  standardsInProccOptions <- processingOptions$`Identifier 1`
  
  # check if all standards are included in the processing options
  for (std in standardsInSampleDesc)
    if (!std %in% standardsInProccOptions) 
      return(p("Input error: The standard '", std, "' is missing from the processing options."))
  
  # check that no standards are in the proccessing options that are not in the sample descr
  for (std in standardsInProccOptions)
    if (!std %in% standardsInSampleDesc) 
      return(p("Input error: The standard '", std, 
               "' is included in the processing options but not in the sample description."))
  
  # check if processing options contain duplicates
  if (any(duplicated(processingOptions$`Identifier 1`)))
    return(p("Input error: The processing options column 'Identifier 1' contains duplicates."))
  
  # check that there are no misssing values in processing options
  if (any(is.na(processingOptions)))
    return(p("Input error: The processing options table contains empty cells."))
  
  # check that control standard is not selected for anything else
  for (row in 1:nrow(processingOptions)){
    isControlStandard <- processingOptions[[row, "Use as control standard"]]
    isUsedForSomethingElse <- processingOptions[[row, "Use for calibration"]] || 
      processingOptions[[row, "Use for drift correction"]]
    isBadEntry <- isControlStandard && isUsedForSomethingElse
    if (isBadEntry) 
      return(p("Input error: The standard '", processingOptions[[row, "Identifier 1"]], 
               "' is selected as control standard. It may not be selected for anything else."))
  }
  
  # sample description and processing options were validated sucessfully
  return(downloadButton(id, "Download sample description", style = blue))
}