library(futile.logger)
library(shiny)

#' goToPage
#'
#' Go to the target page. Display only the target page name and
#' 'Home' on the page header.
#'
#' Note that the variable 'session' (as used by Shiny) needs 
#' to be defined in the server environment.
#' 
#' @param targetPage A character vector. The name of the page to
#'                   go to.
#' @param serverEnvir The environment of app.R's server function.
#'                    Used to execute code in that environment.
#'
#' @return No explicit return value
#'
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
