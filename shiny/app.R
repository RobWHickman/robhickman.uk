library(shiny)
library(DT)

# Read the verb data from CSV
verb_data_full <- read.csv("irish_verbs.csv", stringsAsFactors = FALSE)

# Create a clean dataset with English from column 2, Irish from column 1, importance from column 3
verb_data_full <- data.frame(
  English = verb_data_full[, 2],     # infinitive column (English)
  Irish = verb_data_full[, 1],       # verb column (Irish)
  importance = verb_data_full[, 3],  # importance column
  stringsAsFactors = FALSE
)

# Remove any rows with missing data
verb_data_full <- verb_data_full[complete.cases(verb_data_full), ]

# Function to normalize text (remove accents, convert to lowercase)
normalize_text <- function(text) {
  # Remove accents and convert to lowercase
  text <- iconv(text, to = "ASCII//TRANSLIT")
  text <- tolower(trimws(text))
  return(text)
}

# Function to filter verbs by importance
filter_verbs_by_importance <- function(min_importance) {
  if (min_importance == 0) {
    return(verb_data_full)
  } else {
    return(verb_data_full[verb_data_full$importance >= min_importance, ])
  }
}

# UI
ui <- fluidPage(
  titlePanel("Irish Verb Practice (Sporcle Style)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Settings"),
      radioButtons("importance_filter", 
                   "Minimum Importance Level:",
                   choices = list("All verbs (0)" = 0,
                                  "Importance ≥ 1" = 1,
                                  "Importance ≥ 2" = 2,
                                  "Importance ≥ 3" = 3),
                   selected = 0),
      br(),
      textOutput("verb_count")
    ),
    
    mainPanel(
      h3("Practice"),
      wellPanel(
        h4(textOutput("currentVerb")),
        textInput("userAnswer", "Type the Irish verb:", "", 
                  placeholder = "Enter Irish translation here..."),
        br(),
        actionButton("submitAnswer", "Submit Answer", class = "btn-primary"),
        actionButton("nextVerb", "Skip to Next", class = "btn-secondary"),
        br(), br(),
        h4(textOutput("feedback"))
      ),
      
      br(),
      h4("Your Attempts"),
      DT::dataTableOutput("attemptsTable"),
      
      br(),
      h4("Statistics"),
      verbatimTextOutput("stats")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values
  values <- reactiveValues(
    current_index = 1,
    attempts_df = data.frame(
      Position = integer(),
      English = character(),
      Your_Answer = character(),
      Correct_Answer = character(),
      Status = character(),
      stringsAsFactors = FALSE
    ),
    attempt_number = 1,
    correct_count = 0,
    total_attempts = 0
  )
  
  # Reactive filtered verb data
  filtered_verbs <- reactive({
    filter_verbs_by_importance(as.numeric(input$importance_filter))
  })
  
  # Update verb count display
  output$verb_count <- renderText({
    verb_count <- nrow(filtered_verbs())
    paste("Available verbs:", verb_count)
  })
  
  # Reset when importance filter changes
  observeEvent(input$importance_filter, {
    if (nrow(filtered_verbs()) > 0) {
      values$current_index <- sample(1:nrow(filtered_verbs()), 1)
      values$attempts_df <- data.frame(
        Position = integer(),
        English = character(),
        Your_Answer = character(),
        Correct_Answer = character(),
        Status = character(),
        stringsAsFactors = FALSE
      )
      values$attempt_number <- 1
      values$correct_count <- 0
      values$total_attempts <- 0
      updateTextInput(session, "userAnswer", value = "")
      output$feedback <- renderText("")
    }
  })
  
  # Initialize with first verb
  observeEvent("", {
    if (nrow(filtered_verbs()) > 0) {
      values$current_index <- sample(1:nrow(filtered_verbs()), 1)
    }
  }, once = TRUE)
  
  # Display current verb
  output$currentVerb <- renderText({
    if (nrow(filtered_verbs()) > 0) {
      paste("Verb", values$attempt_number, "of", nrow(filtered_verbs()), ":", filtered_verbs()$English[values$current_index])
    } else {
      "No verbs available for this importance level"
    }
  })
  
  # Handle Enter key in text input
  observeEvent(input$userAnswer, {
    if (input$userAnswer != "" && grepl("[\r\n]", input$userAnswer)) {
      # Enter was pressed, trigger submit
      updateTextInput(session, "userAnswer", value = gsub("[\r\n]", "", input$userAnswer))
      click("submitAnswer")
    }
  })
  
  # Submit answer
  observeEvent(input$submitAnswer, {
    if (input$userAnswer == "") {
      output$feedback <- renderText("Please enter an answer!")
      return()
    }
    
    values$total_attempts <- values$total_attempts + 1
    correct_irish <- verb_data_full$Irish[values$current_index]
    user_answer <- trimws(input$userAnswer)
    
    # Check if correct using normalized text
    is_correct <- normalize_text(user_answer) == normalize_text(correct_irish)
    if (is_correct) {
      values$correct_count <- values$correct_count + 1
      status <- "Correct"
      output$feedback <- renderText("Correct!")
    } else {
      status <- "Incorrect"
      output$feedback <- renderText(paste("Incorrect. The answer was:", correct_irish))
    }
    
    # Add to attempts table
    new_row <- data.frame(
      Position = values$attempt_number,
      English = verb_data_full$English[values$current_index],
      Your_Answer = user_answer,
      Correct_Answer = correct_irish,
      Status = status,
      stringsAsFactors = FALSE
    )
    
    values$attempts_df <- rbind(values$attempts_df, new_row)
    values$attempt_number <- values$attempt_number + 1
    
    # Move to next verb
    values$current_index <- sample(1:nrow(verb_data_full), 1)
    updateTextInput(session, "userAnswer", value = "")
  })
  
  # Skip to next verb
  observeEvent(input$nextVerb, {
    if (nrow(filtered_verbs()) > 0) {
      values$current_index <- sample(1:nrow(filtered_verbs()), 1)
    }
    updateTextInput(session, "userAnswer", value = "")
    output$feedback <- renderText("")
  })
  
  # Attempts table with color coding
  output$attemptsTable <- DT::renderDataTable({
    if (nrow(values$attempts_df) == 0) {
      return(data.frame(Message = "No attempts yet"))
    }
    
    DT::datatable(values$attempts_df, options = list(
      pageLength = 15,
      searching = FALSE,
      paging = FALSE,
      ordering = FALSE,
      columnDefs = list(
        list(targets = 4, visible = FALSE)  # Hide Status column
      )
    )) %>%
      formatStyle(
        "Your_Answer",
        "Status",
        backgroundColor = styleEqual(c("Correct", "Incorrect"), c("#d4edda", "#f8d7da")),
        color = styleEqual(c("Correct", "Incorrect"), c("#155724", "#721c24"))
      ) %>%
      formatStyle(
        "Correct_Answer",
        "Status", 
        backgroundColor = styleEqual(c("Correct", "Incorrect"), c("#d4edda", "#f8d7da")),
        color = styleEqual(c("Correct", "Incorrect"), c("#155724", "#721c24"))
      )
  })
  
  # Statistics
  output$stats <- renderText({
    if (values$total_attempts > 0) {
      percentage <- round((values$correct_count / values$total_attempts) * 100, 1)
      paste("Correct answers:", values$correct_count, "out of", values$total_attempts,
            paste0("(", percentage, "%)"))
    } else {
      "No attempts yet"
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
