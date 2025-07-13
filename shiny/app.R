library(shiny)

# Define UI
ui <- fluidPage(
  h1("Hello Shiny!"),
  p("This is a very simple Shiny app."),
  
  textInput("name", "Enter your name:", "World"),
  textOutput("greeting")
)

# Define server logic
server <- function(input, output) {
  output$greeting <- renderText({
    paste("Hello,", input$name, "!")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
