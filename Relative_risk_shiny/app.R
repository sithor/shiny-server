# Install required packages if not already installed
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("effectsize", quietly = TRUE)) install.packages("effectsize")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("epitools", quietly = TRUE)) install.packages("epitools")
# Load required libraries
library(shiny)
library(oddsratio)
library(epitools)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Visualizing odds and risk ratios"),
  sidebarLayout(
    sidebarPanel(
      numericInput("outcome_exposed", "Outcome in exposed:", value = 20),
      numericInput("no_outcome_exposed", "No outcome in exposed:", value = 30),
      numericInput("outcome_unexposed", "Outcome in unexposed:", value = 10),
      numericInput("no_outcome_unexposed", "No outcome in unexposed:", value = 40),
      actionButton("calculate_btn", "Calculate", icon = icon("calculator"))
    ),
    mainPanel(
      plotOutput("odds_ratio_plot"),
      plotOutput("risk_ratio_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  observeEvent(input$calculate_btn, {
    # Create a 2x2 table
    tab <- matrix(c( input$no_outcome_unexposed,input$no_outcome_exposed,
                     input$outcome_unexposed, input$outcome_exposed),
                  ncol = 2)
    
    # Calculate odds ratios
    or <- epitools::epitab(tab, method = "oddsratio")
    rr <- epitools::epitab(tab, method = "riskratio")
    dfr <- rr$tab |> data.frame()
    df <- or$tab |> data.frame()
    df <- df[-1,]
    dfr <- dfr[-1,]
    # Plot the odds ratio using plot_odds_ratio
    p <- ggplot(df, aes(x = oddsratio, y = 1))
    p <- p + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
      geom_errorbarh(aes(xmax = upper, xmin = lower), size = .5, height = .2, color = "gray50") +
      geom_point(size = 3.5, color = "orange") +
      theme_bw() +
      theme(panel.grid.minor = element_blank()) +
      scale_y_continuous(breaks = 1) +
      scale_x_continuous(breaks = seq(0,7,0.5) ) +
      coord_trans(x = "log10") +
      ylab("") +
      xlab("odds ratio (log scale)") +
      annotate(geom = "text", y =1.1, x = 1, label = paste0("Odds ratio: ", round(df$oddsratio, 2),
                                                             "; 95% CI:", round(df$lower, 2), " to ", 
                                                             round(df$upper, 2)), size = 3.5, hjust = 0) 
    
    
    q <- ggplot(dfr, aes(x = riskratio, y = 1))
    q <- q + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
      geom_errorbarh(aes(xmax = upper, xmin = lower), size = .5, height = .2, color = "gray50") +
      geom_point(size = 3.5, color = "orange") +
      theme_bw() +
      theme(panel.grid.minor = element_blank()) +
      scale_y_continuous(breaks = 1) +
      scale_x_continuous(breaks = seq(0,7,0.5) ) +
      coord_trans(x = "log10") +
      ylab("") +
      xlab("risk ratio (log scale)") +
      annotate(geom = "text", y =1.1, x = 1, label = paste0("Risk ratio: ", round(dfr$riskratio, 2),
                                                            "; 95% CI:", round(dfr$lower, 2), " to ", 
                                                            round(dfr$upper, 2)), size = 3.5, hjust = 0) 
    
    
    output$odds_ratio_plot <- renderPlot({
      p
    })
    output$risk_ratio_plot <- renderPlot({
      q
    })
  })
}

# Run the Shiny app
shinyApp(ui, server)