library(shiny)

ui <- fluidPage(
  tags$h1("Welcome to Cpt. Picarr")
)

server <- function(input, output){}

shinyApp(ui, server)