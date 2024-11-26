# Shiny app to simulate some custom dice for a home-brew x-wing miniature
# Sam Siljee
# 10 November 2024

# Packages
library(dplyr)
library(shiny)

# Set the UI
ui <- fluidPage(
  tabsetPanel(
    tabPanel(
      "Deep Sea Adventure Boost",
      # Button to roll the dice
      actionButton("ds_roll", "Roll the dice"),
      
      # Display the results
      h3("Results:"),
      textOutput("ds_results"),
      tags$style(type="text/css", "#results {white-space: pre-wrap;}")
    ),
    tabPanel(
      "Custom x-wing dice",
      
      # Button to roll the dice
      actionButton("roll", "Roll the dice"),
      
      # Display the results
      h3("Results:"),
      textOutput("results"),
      tags$style(type="text/css", "#results {white-space: pre-wrap;}"),
      hr(),
      
      # Show possible dice outcomes
      tableOutput("dice")
    )
  )
)

# Make the server
server <- function(input, output) {
  
  # Load the data
  dice <- read.delim("Dice.tsv", header = TRUE)
  
  # Initialise blank vector for results
  results <- reactiveVal("Click roll the dice")
  
  # Event to roll the dice
  observeEvent(input$roll, {
    # Initialise blank results vector
    roll_results <- vector()
    
    # Sample from the dice data
    for(die in colnames(dice)){
      # Sample from the die
      result <- as.character(sample(dice[[die]], 1))

      # Add to results
      roll_results <- c(roll_results, result)
    }
    
    # Update the results
    results(paste(sort(roll_results), collapse = "\n"))
  })
  
  # Initialise blank vector for ds results
  ds_results <- reactiveVal("Click roll the dice")
  
  # Event to roll the dice
  observeEvent(input$ds_roll, {
    # Update the results
    ds_results(
      paste(
        sample(c("One", "Two"), 1),
        sample(c("One", "Three"), 1),
        sample(c("Zero", "Three"), 1),
        collapse = "\n"
      )
    )
  })
  
  # Render results
  output$results <- renderText(
    results()
  )
  
  output$ds_results <- renderText(
    ds_results()
  )
  
  # Render table of dice
  output$dice <- renderTable(
    dice
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
