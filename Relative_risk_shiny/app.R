# Install required packages if not already installed
# if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
# if (!requireNamespace("effectsize", quietly = TRUE)) install.packages("effectsize")
# if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
# if (!requireNamespace("epitools", quietly = TRUE)) install.packages("epitools")
# Load required libraries

## I'm adding stuff
library(shiny)
#ßlibrary(oddsratio)
library(epitools)
library(ggplot2)
library(shinythemes)
library(eulerr)

one_dp <- function(x){
  formatC(x, digits = 1, format = "f")
}

two_dp <- function(x){
  formatC(x, digits = 2, format = "f")
}

three_dp <- function(x){
  formatC(x, digits = 3, format = "f")
}

# Define UI
ui <- fluidPage(theme = shinytheme("cyborg"),
                tags$head(
                  tags$style(HTML("
                                  .shiny-output-error-validation {
                                  color: green;
                                  }
                                  "))),
                
                tags$head(
                  tags$style(HTML("
                        @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                        
                        h1 {
                        font-family: 'Lobster', cursive;
                        font-weight: 500;
                        line-height: 1.1;
                        color: #33ccff;
                        }
                        
                        "))),
                
                tags$head(
                  tags$style(HTML("
                        @import url('https://fonts.googleapis.com/css?family=Frank+Ruhl+Libre');
                        
                        body {
                        font-family: 'Frank Ruhl Libre', serif;
                        font-weight:  50;
                        line-height: 1.1;
                        color: #FFCBB8;
                        }
                        
                        "))),
                
                tags$head(tags$script('
                        var dimension = [0, 0];
                                      $(document).on("shiny:connected", function(e) {
                                      dimension[0] = window.innerWidth;
                                      dimension[1] = window.innerHeight;
                                      Shiny.onInputChange("dimension", dimension);
                                      });
                                      $(window).resize(function(e) {
                                      dimension[0] = window.innerWidth;
                                      dimension[1] = window.innerHeight;
                                      Shiny.onInputChange("dimension", dimension);
                                      });
                                      ')),
  tabsetPanel(
  tabPanel("Input data",
  
  headerPanel("Visualizing odds and risk ratios
              \n\n from a 2x2 table"),
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
      plotOutput("risk_ratio_plot"),
      br(), br(),
      textOutput("rr_text"),
      br(), br(),
      textOutput("or_text"),
    )
  )
  ),
tabPanel("Euler plot",
  mainPanel(
    plotOutput("euler_plot")
  )
)))

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
    
    PAR <-(100*(dfr$p0*(dfr$riskratio - 1))/(1 + dfr$p0*(dfr$riskratio - 1))) |> one_dp()
    PAR <- paste0(PAR, "%")
    
    RR_text <- paste0("\n\nRisk ratio: ", dfr$riskratio |> two_dp(),
           "; 95% CI: ", dfr$lower |> two_dp(), " to ", 
           dfr$upper |> two_dp(), "\n P",
           ifelse(df$p.value < 0.001, " < 0.001",
                  paste0(" = ", df$p.value |> three_dp())),"; PAR: ", PAR)
    
    OR_text <- paste0("\n\nOdds ratio: ", df$oddsratio |> two_dp(),
                      "; 95% CI: ", df$lower |> two_dp(), " to ", 
                      df$upper |> two_dp(), "\n P",
                      ifelse(df$p.value < 0.001, " < 0.001",
                             paste0(" = ", df$p.value |> three_dp())),"; PAR: ", PAR)
    
    # Plot the odds ratio using plot_odds_ratio
    p <- ggplot(df, aes(x = oddsratio, y = 1))
    p <- p + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
      geom_errorbarh(aes(xmax = upper, xmin = lower), size = .5, height = .1, color = "gray50") +
      geom_point(size = 3.5, color = "orange") +
      theme_bw() +
      theme(panel.grid.minor = element_blank()) +
      scale_y_continuous(breaks = 1) +
      scale_x_continuous(breaks = seq(0,7,0.5) ) +
      coord_trans(x = "log10") +
      ylab("") +
      xlab("odds ratio (log scale)") +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold")) +
      annotate(geom = "text", y =1.1, x = 1, label = paste0("\n\nOdds ratio: ", df$oddsratio |> two_dp(),
                                                             "; 95% CI: ", df$lower |> two_dp(), " to ", 
                                                            df$upper |> two_dp(), "\n P",
                                                            ifelse(df$p.value < 0.001, " < 0.001",
                                                                   paste0(" = ", df$p.value |> three_dp())),"; PAR: ", PAR), size = 10, hjust = 0) 
    
    
    q <- ggplot(dfr, aes(x = riskratio, y = 1))
    q <- q + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
      geom_errorbarh(aes(xmax = upper, xmin = lower), size = .5, height = .1, color = "gray50") +
      geom_point(size = 3.5, color = "orange") +
      theme_bw() +
      theme(panel.grid.minor = element_blank()) +
      scale_y_continuous(breaks = 1) +
      scale_x_continuous(breaks = seq(0,7,0.5) ) +
      coord_trans(x = "log10") +
      ylab("") +
      xlab("risk ratio (log scale)") +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold")) +
      annotate(geom = "text", y =1.1, x = 1, label = paste0("\n\nRisk ratio: ", dfr$riskratio |> two_dp(),
                                                            "; 95% CI: ", dfr$lower |> two_dp(), " to ", 
                                                            dfr$upper |> two_dp(), "\n P",
                                                            ifelse(df$p.value < 0.001, " < 0.001",
                                                                  paste0(" = ", df$p.value |> three_dp())),"; PAR: ", PAR), 
                                                              size = 10, hjust = 0)
    
    
    fit1 <- euler(c("exposed&all" = input$no_outcome_exposed,
                    "outcome&all" =  input$outcome_unexposed,
                    "exposed&outcome&all" = input$outcome_exposed,
                    "all" = input$no_outcome_unexposed))
    
    
    output$euler_plot <- renderPlot({
      plot(fit1, quantities = TRUE, legend = TRUE, labels = TRUE)
    })
    
                  
              
    output$odds_ratio_plot <- renderPlot({
      p
    })
    output$risk_ratio_plot <- renderPlot({
      q
    })
    
    output$or_text <- renderText({
      OR_text
    })
    
    output$rr_text <- renderText({
      RR_text
    })
  })
}

# Run the Shiny app
shinyApp(ui, server)