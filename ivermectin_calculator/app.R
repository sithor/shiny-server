# Load required library
library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("💊 Drug dose calculator for scabies."),
  tags$br(),
  tags$img(src='strometol.jpeg', align="right"), 
  tags$br(),
  tags$b("This app is currently in development and is currently not authorised for clinical use."),
  tags$br(),
  "The following app calculates", tags$b("ivermectin dose"), "for children and adults 
  more than 15 kg. 
  Children weighing less than 15 kg and pregnant women are recommended not 
  to take ivermectin, but instead use",
  tags$b("5% topical permethrin lotion"), ".",
  "The calculation rounds the dose up to the nearest 3 mg tablet dose.
  The dose is",
  tags$b("200mcg/kg body weight"),"up to a maximum of 85kg.",
  sidebarLayout(
    sidebarPanel(
      numericInput("members", "🏡 Number of Household Members:", 
                   value = 1, min = 1, step = 1, width = '100px'),
      
      # Dynamic UI for inputting weights
      uiOutput("weightInputs"),
      
      # Button to calculate dose
      actionButton("calculate", "🧮 Recommend treatment")
    ),
    
    mainPanel(
      h3("Results:"),
      "Ivermectin should be taken with a fatty meal for maximum absorption.",
      tableOutput("doseTable"),
     htmlOutput("summary")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Generate dynamic inputs for weights and pregnancy status
  output$weightInputs <- renderUI({
    n <- input$members
    inputs <- lapply(1:n, function(i) {
      fluidRow(
        column(6, numericInput(paste0("weight", i), paste("️⚖️ Weight of member", 
                        i, " (kg):"), value = 50, min = 0,
                               width = '100px')),
        column(6, checkboxInput(paste0("pregnant", i), 
                            paste("🤰", i,
                            "pregnant?"), value = FALSE))
      )
    })
    inputs
  })
  
  # Observe button click to calculate doses
  observeEvent(input$calculate, {
    n <- input$members
    results <- lapply(1:n, function(i) {
      weight <- input[[paste0("weight", i)]]
      pregnant <- input[[paste0("pregnant", i)]]
      
      # Calculate dose or recommend alternative treatment
      if (pregnant) {
        return(c(paste("Member", i), weight, "5% permethrin lotion recommended (pregnant), 30g tube, repeated after 7-14 days."))
      } else if (weight < 15) {
        return(c(paste("Member", i), weight, "5% permethrin lotion recommended (< 15kg), 30g tube, repeated after 7-14 days."))
      } else if (weight > 85 ) {
        dose <- 85 * 0.2
      } else {
        dose <- weight * 0.2 # 200 mcg/kg
      }
      return(c(paste("Member", i), weight, paste(round(dose, 2), "mg of ivermectin, 
                                                 which is",
                                                 ceiling(dose/3),
                  "tablets (3mg) as a stat. dose repeated after 7-14 days")))
    })
    
    # Convert results to data frame for display
    dose_df <- do.call(rbind, results)
    colnames(dose_df) <- c("Household Member", "Weight (kg)", "Recommendation/Dose")
    
    output$doseTable <- renderTable({
      dose_df
    })
    
    # Summary of alternative recommendations
    summary_text <- paste(sum(sapply(1:n, function(i) {
      weight <- input[[paste0("weight", i)]]
      pregnant <- input[[paste0("pregnant", i)]]
      pregnant || weight < 15
    })), "members recommended permethrin lotion.")
    
    output$summary <- renderText({
      summary_text
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)