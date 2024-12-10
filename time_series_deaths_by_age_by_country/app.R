#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Install required packages if not already installed
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("forecast", quietly = TRUE)) install.packages("forecast")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
if (!requireNamespace("rio", quietly = TRUE)) install.packages("rio")
library(shiny)
library(forecast)
library(rio)

# Load necessary libraries
library(shiny)
library(readxl)
library(forecast)
library(ggplot2)
library(dplyr)
library(stringr)

# Preload the Excel file
data_path <- "./data/stmf.xlsx"  # Adjust the file path as needed
all_sheets <- excel_sheets(data_path)
all_sheets <- all_sheets[-1]
Country_code <- rio::import("./data/Country.xlsx")

# Define the UI
ui <- fluidPage(
  titlePanel("Time-series analysis of recent all-cause mortality trends by age and country"),
  "This app conducts a seasonal trend-decomposition on all-cause
  mortality and count data, by age group and country, from the following",
  tags$a(href="https://mpidr.shinyapps.io/stmortality/", " website."),
  "Vertical red lines indicate the 2021 year.",
  br(), br(),
  sidebarLayout(
    sidebarPanel(
      selectInput("sheet", "Select Country (Sheet)", choices = Country_code$Country),
      selectInput("age_category", "Select Age Category", choices = NULL),
      selectInput("data_type", "Select Data Type", choices = c("Count" = "count", "Rate" = "rate")),
      actionButton("plot", "Plot")
    ),
    mainPanel(
      plotOutput("time_series_plot")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Load the selected sheet's data
  data <- reactive({
    req(input$sheet)
    df <- rio::import(data_path, sheet = Country_code[Country_code$Country == input$sheet, "Code"], skip = 2)
    # Rename columns for consistency
    names(df)[5:10] <- paste0("count_", gsub("\\..*", "", names(df)[5:10]))
    names(df)[11:16] <- paste0("rate_", gsub("\\..*", "", names(df)[11:16]))
    df <- df[df$Sex == "b", ] # Filter for "both sexes"
    return(df)
  })
  
  # Update age category choices based on the data
  observeEvent(data(), {
    req(data())
    cols <- names(data())
    age_categories <- unique(str_extract(cols, "0-14|15-64|65-74|75-84|85\\+"))
    age_categories <- age_categories[-1]
    age_categories <- c("Total", age_categories)
    updateSelectInput(session, "age_category", choices = age_categories)
  })
  
  # Render the time series plot
  output$time_series_plot <- renderPlot({
    req(input$plot)
    req(data(), input$age_category, input$data_type)
    
    # Create column name based on user selection
    column_name <- paste0(input$data_type, "_", input$age_category)
    if (!column_name %in% names(data())) {
      showNotification("Invalid selection. Column not found.", type = "error")
      return(NULL)
    }
    
    # Extract and process time series
    start_date <- data()$Year[1] + (data()$Week[1] -1)/52
    time_series <- ts(data()[[column_name]], frequency = 52, start = start_date)
    decomposition <- stl(time_series, "periodic")
    age_cat <- str_split(column_name, '_')
    
    # Plot the decomposition
    forecast::autoplot(decomposition) +
      ggtitle(paste0("Time-series analysis of ", input$data_type, "s of deaths in ",
                     input$sheet, "\n", age_cat[[1]][2], " years")) +
      geom_vline(xintercept = c(2021, 2022), linetype = "dotted", 
                 size = 0.7, colour = "red")
  })
}

# Run the application
shinyApp(ui = ui, server = server)