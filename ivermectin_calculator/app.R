# Load required library
library(shiny)
library(htmlTable)
# Define UI
ui <- fluidPage(
  titlePanel("💊 Drug dose calculator for scabies"),
  tags$br(),
  tags$img(src='strometol.jpeg', align="right"), 
  tags$br(),
  tags$b("This app is currently in development and is currently not authorised 
         for clinical use."),
  tags$br(),
  tags$br(),
  "In New Zealand, at present, topical permethrin is the first line treatment",
   "The following app helps the clinician assess possible scabies treatment and 
   calculates", tags$b("ivermectin dose"), "for children and adults 
  more than 15 kg, along with taking the clinician through the NZ special
  authority criteria.",
  "Children weighing less than 15 kg and pregnant women are recommended not 
  to take ivermectin, but instead use",
  tags$b("5% topical permethrin lotion"), ".",
  "The calculation rounds the dose up to the nearest 3 mg tablet dose.
  The dose is",
  tags$b("200mcg/kg body weight"),"up to a maximum of 85kg.",
  tags$br(),
  tags$br(),
  sidebarLayout(
    sidebarPanel(
      h3("Special authority criteria for funded ivermectin"),
      "Ivermectin is govt. funded if the patient has any of the following:",
      checkboxInput("crusted",  
                    "Crusted scabies?", value = FALSE),
      checkboxInput("topical",  
                    "Unable to comply with topical permethrin?", 
                    value = FALSE),
      checkboxInput("previous",  
                    "Failure of topical permethrin?", value = FALSE),
      numericInput("members", "🏡 Number of Household Members:", 
                   value = 3, min = 1, max = 30, step = 1, width = '80px'),
      conditionalPanel(
        condition="input.crusted==true | input.topical==true |
        input.previous==true",
        "Please apply for special authority to use ivermectin",
        tags$a(href="https://schedule.pharmac.govt.nz/2024/10/01/SA2294.pdf", 
        "here.", target="_blank"),
        tags$br(),
        tags$br()
      ),
        
      # Dynamic UI for inputting weights
      uiOutput("weightInputs"),
      
      # Button to calculate dose
      actionButton("calculate", "🧮 Recommend treatment")
    ),
    
    mainPanel(
      h3("Results:"),
      conditionalPanel(
        condition="input.crusted==true | input.topical==true |
        input.previous==true",
        HTML(paste("Ivermectin should be taken with a ",
        tags$b("fatty meal"), "for maximum absorption."))),
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
                        i, "(kg):"), value = 50, min = 0, max = 300,
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
      if(!(input$crusted==TRUE | input$topical==TRUE |
         input$previous==TRUE)){
        return(c(i, weight, paste("5% permethrin lotion recommended", tags$b("(no special authority criteria),"), 
                 "30g tube, repeated after 7-14 days.")))
      }
      
      
      if (pregnant) {
        return(c(i, weight, "5% permethrin lotion recommended", tags$b("(pregnant),"), 
                 "30g tube, repeated after 7-14 days."))
      } else if (weight < 15) {
        return(c(i, weight, "5% permethrin lotion recommended", tags$b("(less than 15kg),"), 
                 "30g tube, repeated after 7-14 days."))
      } else if (weight > 85 ) {
        dose <- 85 * 0.2
      } else {
        dose <- weight * 0.2 # 200 mcg/kg
      }
      return(c( i, weight, paste(tags$b(round(dose, 2)), "mg of ivermectin, 
                                                 which is",
                                                 tags$b(ceiling(dose/3)),
                  "tablets (3mg) as stat. dose repeated after 7-14 days")))
    })
    
    # Convert results to data frame for display
    dose_df <- do.call(rbind, results)
    colnames(dose_df) <- c("Member", "Weight (kg)", "Recommendation/Dose")
    selectedData <- eventReactive(input$calculate, {
      
      # Create the table (using table from htmlTables doc as example)
      HTML(
        htmlTable(dose_df,
                  css.cell = "padding: 30px; border-bottom: 2px solid black;") # Styling to separate headers) 
      )
      
    })
    
    output$doseTable <- renderUI({selectedData()})
    # output$doseTable <- htmlTable({
    #   dose_df
    # })
    
    
    
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