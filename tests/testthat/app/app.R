library(shiny)
devtools::load_all()

shinyApp(ui = cptPicarr:::ui, server = cptPicarr:::server)