
## This app is for outputting the results of a 2x2 table with interpretation of the results.

## by Simon Thornley


# Install required packages if not already installed
# if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
# if (!requireNamespace("effectsize", quietly = TRUE)) install.packages("effectsize")
# if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
# if (!requireNamespace("epitools", quietly = TRUE)) install.packages("epitools")
# Load required libraries

library(shiny)
library(epitools)
library(ggplot2)
library(shinythemes)
library(eulerr)
library(mekko)
library(epiR)
library(fmsb)
source("functions.R") #inzight plot


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
      mainPanel(
      "Please input data and press 'calculate'.\n",
      fluidRow(
        column(width = 3, h3("Exposed?")),
        column(width = 2, h4("Outcome", style="color:red")),
        column(width = 2, h4("No outcome", style="color:#90EE90"))),
        fluidRow(
          column(width = 3, h4("Exposed", style="color:orange")),
          column(width = 2, numericInput("outcome_exposed", "", value = 20)),
          column(width = 2, numericInput("no_outcome_exposed", "", value = 30))),
        fluidRow(
          column(width = 3, h4("Not exposed", style="color:#ADD8E6")),
          column(width = 2, numericInput("outcome_unexposed", "", value = 10)),
          column(width = 2, numericInput("no_outcome_unexposed", "", value = 40))),
        actionButton("calculate_btn", "Calculate", icon = icon("calculator")),
      plotOutput("odds_ratio_plot"),
      plotOutput("risk_ratio_plot"),
      br(), br(),
      h4("Risk ratio; Population attributable risk; P-value"),
      htmlOutput("rr_text"),
      textOutput("rr_desc"),
      textOutput("par_desc"),
      htmlOutput("p_desc"),
      br(), br(),
      h4("Odds ratio"),
      htmlOutput("or_text"),
      textOutput("or_desc"),
      br(), br(),
      h4("Risk difference"),
      htmlOutput("rd_text"),
      br(), br(),
      h4("Number needed to treat"),
      htmlOutput("nnt_text"),
      br(), br(),
      "Note: PAR is population attributable risk, note this is assumed to be a 
      cohort or cross-sectional study, where the relevant prevalence of exposure
      is the unconditional.",
      br(), br(),
      "Confidence interval for population attributable risk
      is estimated using the ", 
      a("delta method.", 
      href="https://onlinelibrary.wiley.com/doi/abs/10.1002/%28SICI%291097-0258%2820000430%2919%3A8%3C1089%3A%3AAID-SIM411%3E3.0.CO%3B2-0")
    )
  ),
tabPanel("Euler plot",
  mainPanel(
    plotOutput("euler_plot")
  )),
tabPanel("Bar plot",
           mainPanel(
             plotOutput("inzight_plot")
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
    
    RD <- fmsb::riskdifference(input$outcome_exposed, input$outcome_unexposed,
                               input$outcome_exposed + input$no_outcome_exposed,
                               input$outcome_unexposed +  input$no_outcome_unexposed,
                               conf.level = 0.95)
    
    RD_text <- paste0("\n\nRisk difference: ", RD$estimate |> two_dp(),
                      "; 95% CI: ", RD$conf.int[1] |> two_dp(), " to ", 
                      RD$conf.int[2] |> two_dp(), ";\n <em>P</em>",
                      ifelse(df$p.value < 0.001, " < 0.001",
                             paste0(" = ", (df$p.value) |> three_dp())))
    
    NNT_text <- paste0("\n\nNumber needed to treat: ", (1/RD$estimate) |> round(0),
                       "; 95% CI: ", (1/RD$conf.int[2]) |> round(0), " to ", 
                       (1/RD$conf.int[1]) |> round(0), ";\n <em>P</em>",
                       ifelse(df$p.value < 0.001, " < 0.001",
                              paste0(" = ", (df$p.value) |> three_dp())))
    
    
    #PAR <-(100*(dfr$p0*(dfr$riskratio - 1))/(1 + dfr$p0*(dfr$riskratio - 1))) |> one_dp()
    #PAR <- paste0(PAR, "%")
    epiR_tab <-  cbind(tab[,2], tab[,1]) |> as.table()
    epiR_tab <-  rbind(epiR_tab[2,], epiR_tab[1,]) |> as.table()
    PAR_alt <- epiR::epi.2by2(epiR_tab,
                        method="cohort.count",
                        digits=2,
                        conf.level=0.95, 
                        units=100,
                        interpret=TRUE,
                        outcome="as.columns")
    
    #browser()
     PAR <- paste0((PAR_alt$massoc.detail$PAFRisk.strata.wald[[1]]*100) |> one_dp(), "% (95% CI: ",
                       (PAR_alt$massoc.detail$PAFRisk.strata.wald[[2]]*100) |> one_dp(), " to ",
                       (PAR_alt$massoc.detail$PAFRisk.strata.wald[[3]]*100) |> one_dp(), ")")
  
    
    RR_text <- paste0("\n\nRisk ratio: ", dfr$riskratio |> two_dp(),
           "; 95% CI: ", dfr$lower |> two_dp(), " to ", 
           dfr$upper |> two_dp(), ";\n <em>P</em>",
           ifelse(df$p.value < 0.001, " < 0.001",
                  paste0(" = ", df$p.value |> three_dp())),"; PAR: ", PAR)
    
    if ( df$oddsratio > 1){
      OR_desc <- paste0("The odds of the outcome is ", df$oddsratio |> one_dp(),
                        " times higher in the exposed group, compared to the unexposed group.")
    } else if (df$oddsratio < 1) {
      OR_desc <- paste0("The odds of the outcome is ", (100*(1- df$oddsratio)) |> one_dp(),
                        "% lower in the exposed group, compared to the unexposed group.")
    } else {
      OR_desc <- paste0("The odds of the outcome is the same in the exposed and unexposed groups.")
    }
    
    if ( dfr$riskratio > 1){
      RR_desc <- paste0("The risk of the outcome is ", dfr$riskratio |> one_dp(),
                        " times higher in the exposed group, compared to the unexposed group.")
    } else if (dfr$riskratio < 1) {
      RR_desc <- paste0("The risk of the outcome is ", (100*(1- dfr$riskratio)) |> one_dp(),
                        "% lower in the exposed group, compared to the unexposed group.")
    } else {
      RR_desc <- paste0("The risk of the outcome is the same in the exposed and unexposed groups.")
    }
    
    
    OR_text <- paste0("\n\nOdds ratio: ", df$oddsratio |> two_dp(),
                      "; 95% CI: ", df$lower |> two_dp(), " to ", 
                      df$upper |> two_dp(), ";\n <em>P</em>",
                      ifelse(df$p.value < 0.001, " < 0.001",
                             paste0(" = ", df$p.value |> three_dp())),"; PAR: ", PAR)
    
    if ( PAR > 0){
    PAR_desc <- paste0("If the exposure were taken away completely from the 
    population (and the exposure causes disease), ", PAR, " of cases in the 
                       population would be prevented.")
    } else if (PAR < 0) {
      PAR_desc <- paste0("If the exposure were taken away completely from the 
    population (and the exposure prevents disease), the percentage of cases in 
    the population would increase by ", PAR, " of cases in the 
                       population would be prevented.")
    } else {
      PAR_desc <- paste0("No change in cases is expected if the exposure is taken away.")
    }
    
    if ( df$p.value >= 0.05){
      P_desc <- paste0("The <em>P</em>-value of ",  df$p.value |> three_dp(),
                       " is the probability of the observed results or more extreme
                        assuming exposure and outcome are unrelated (independent).
                       The result is <strong>not statistically significant</strong> and it is 
                       concluded that exposure and disease are not associated.")
    } else {
      P_desc <- paste0("The <em>P</em>-value of ",  
                       ifelse(df$p.value < 0.001, " < 0.001",
                              paste0(" = ", df$p.value |> three_dp())),
                       " is the probability of the observed results or more extreme
                        assuming exposure and outcome are unrelated (independent).
                       The result is <strong>statistically significant</strong> and it is 
                       concluded that exposure and disease are associated.")
    }
    
    
    
    
    
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
    
    
    fit1 <- euler(c("exposed" = input$no_outcome_exposed,
                    "outcome&unexposed" =  input$outcome_unexposed,
                    "exposed&outcome" = input$outcome_exposed,
                    "unexposed" = input$no_outcome_unexposed))
    
    
    output$euler_plot <- renderPlot({
      plot(fit1, quantities = TRUE, legend = TRUE, labels = FALSE)
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
    
    output$rr_desc <- renderText({
      RR_desc
    })
    
    output$or_desc <- renderText({
      OR_desc
    })
    
    output$p_desc <- renderText({
      P_desc
    })
    output$rd_text <- renderText({
      RD_text
    })
    
    output$nnt_text <- renderText({
      NNT_text
    })
    
    output$par_desc <- renderText({
      PAR_desc
    })
    
    dimnames(tab) <- list(c("No","Yes"), c("No","Yes"))
    names(dimnames(tab)) <- c("Exposure", "Disease")
    dfi <-  expand.table(tab)
   i <- inzight_plot(exposure = "Exposure", 
                             outcome = "Disease",
                             event = "Yes", data = dfi, x_axis_label = "Exposed?",
                             y_axis_label = "Proportion with outcome")
    output$inzight_plot <- renderPlot({
    i
    })
    
  })
}

# Run the Shiny app
shinyApp(ui, server)