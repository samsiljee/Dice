# Shiny app to simulate some custom dice for a home-brew x-wing miniature, Deeps sea adventure, and a car reference for Ascension
# Sam Siljee
# 21 February 2025

# Packages
library(dplyr)
library(shiny)
library(DT)

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
      tags$style(type = "text/css", "#results {white-space: pre-wrap;}")
    ),
    tabPanel(
      "Custom x-wing dice",

      # Button to roll the dice
      actionButton("roll", "Roll the dice"),

      # Display the results
      h3("Results:"),
      textOutput("results"),
      tags$style(type = "text/css", "#results {white-space: pre-wrap;}"),
      hr(),

      # Show possible dice outcomes
      tableOutput("dice")
    ),
    tabPanel(
      "Ascension card reference",

      # Select faction and type input
      radioButtons(
        "faction",
        "Faction",
        choices = c(
          "Enlightened",
          "Lifebound",
          "Mechana",
          "Void",
          "Monster"
        ),
        inline = TRUE
      ),
      radioButtons(
        "type",
        "Type",
        choices = c(
          "Hero",
          "Construct",
          "Monster"
        ),
        inline = TRUE
      ),

      # Select card name
      uiOutput("name"),
      
      # Show translation
      textOutput("effect"), br(),
      textOutput("energize"), br(),
      textOutput("fate"), br(),
      textOutput("trophy"), br(),

      # Show all cards in table format
      DTOutput("ascension_cards")
    ),
    tabPanel(
      "Warcraft extra harvest dice",
      # Button to roll the dice
      actionButton("harvest_roll", "Roll the dice"),
      
      # Button to select how many dice to roll
      numericInput(
        "harvest_count",
        "Number of dice to roll",
        value = 1,
        step = 1),
      
      # Display the results
      h3("Results:"),
      textOutput("harvest_results"),
      tags$style(type = "text/css", "#results {white-space: pre-wrap;}")
    )
  )
)

# Make the server
server <- function(input, output, session) {
  # Load the data
  dice <- read.delim("Dice.tsv", header = TRUE)
  ascension_cards <- read.delim("Ascension_cards.tsv", header = TRUE)
  harvest_die <- c(rep(1, 3), rep(2, 2), "3(x)")

  ## X-wing dice ----
  
  # Initialise blank vector for results
  results <- reactiveVal("Click roll the dice")

  # Event to roll the dice
  observeEvent(input$roll, {
    # Initialise blank results vector
    roll_results <- vector()

    # Sample from the dice data
    for (die in colnames(dice)) {
      # Sample from the die
      result <- as.character(sample(dice[[die]], 1))

      # Add to results
      roll_results <- c(roll_results, result)
    }

    # Update the results
    results(paste(sort(roll_results), collapse = "\n"))
  })
  
  ## Deep sea adventure boost ----

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
  
  ## Ascension cards ----
  
  # Results for ascension cards
  effect <- reactive({
    paste0(
      "Effect: ",
      filter(ascension_cards, Name == input$name) %>% pull(Effect)
    )
  })
  
  energize <- reactive({
    filter(ascension_cards, Name == input$name) %>% pull(Energize)
  })
  
  fate <- reactive({
    if(!is.na(pull(filter(ascension_cards, Name == input$name), Fate))) {
      paste0(
        filter(ascension_cards, Name == input$name) %>% pull(Fate)
      ) 
    }
  })
  
  trophy <- reactive({
    if(!is.na(pull(filter(ascension_cards, Name == input$name), Trophy))) {
      paste0(
        filter(ascension_cards, Name == input$name) %>% pull(Trophy)
      )
    }
  })
  
  # Update radio buttons if Monster selected
  observeEvent(input$type, {
    if(input$type == "Monster") {
      updateRadioButtons(session, "faction", selected = "Monster")
    }
  })
  observeEvent(input$faction, {
    if(input$faction == "Monster") {
      updateRadioButtons(session, "type", selected = "Monster")
    }
  })
  
  # Render reactive UI
  # List of choices for name
  name_choices <- reactive({
    ascension_cards %>%
      filter(Faction == input$faction & Type == input$type) %>%
      pull(Name)
  })
  
  # Select name
  output$name <- renderUI(
    selectInput(
      "name",
      "Name",
      choices = name_choices()
    )
  )

  # Render results
  output$results <- renderText(
    results()
  )

  output$ds_results <- renderText(
    ds_results()
  )
  
  output$effect <- renderText(
    effect()
  )
  
  output$energize <- renderText(
    energize()
  )
  
  output$fate <- renderText(
    fate()
  )
  
  output$trophy <- renderText(
    trophy()
  )

  # Render tables
  output$dice <- renderTable(
    dice
  )
  output$ascension_cards <- renderDT(
    ascension_cards
  )
  
  ## Warcraft harvest dice ----
  # Initialise blank vector for ds results
  harvest_results <- reactiveVal("Click roll the dice")
  
  # Event to roll the dice
  observeEvent(input$harvest_roll, {
    roll_results <- vector()
    
    # Iterate for number of dice selected
    for( i in 1:input$harvest_count) {
      roll_results <- c(
        roll_results,
        sample(harvest_die, 1)
      )
    }
    
    # Update the results
    harvest_results(
      roll_results
    )
  })
  
  # Render results
  output$harvest_results <- renderText(
    harvest_results()
  )
}

# Run the application
shinyApp(ui = ui, server = server)
