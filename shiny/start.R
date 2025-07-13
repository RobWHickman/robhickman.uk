# Install shiny if not available
if (!require("shiny", quietly = TRUE)) {
  install.packages("shiny")
  library(shiny)
}

# Get port from environment (Render provides this)
port <- as.numeric(Sys.getenv("PORT", "3000"))

# Run the app
shiny::runApp(
  appDir = ".",
  host = "0.0.0.0", 
  port = port
)
