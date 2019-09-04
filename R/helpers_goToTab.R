goToTab <- function(target, session, serverEnvir){
  assign(".tabId", target, envir = serverEnvir)
  evalq(
    updateTabsetPanel(session = session, inputId = "app", selected = .tabId), 
    envir = serverEnvir
  )
}