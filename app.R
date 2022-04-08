library(shiny)

ui <- fluidPage(
  # selectInput("variable", label = "Variable", choices = c("LA", "DC"))
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)