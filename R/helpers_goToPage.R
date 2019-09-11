library(futile.logger)
library(shiny)

goToPage <- function(targetPage, serverEnvir){
  
  flog.info("go to page '%s'", targetPage)
  
  # make the value of targetPage accessible in the server envrionment
  assign(".targetPage", targetPage, envir = serverEnvir)
  
  evalq({
    
    hideTab("app", target = "Project", session = session)
    hideTab("app", target = "Generate a sample description", session = session)
    hideTab("app", target = "Upload measurement data", session = session)
    hideTab("app", target = "Process measurement data", session = session)
    
    showTab("app", target = .targetPage, session = session)
    
    updateTabsetPanel(session = session, inputId = "app", selected = .targetPage)
    
  }, envir = serverEnvir)
}
