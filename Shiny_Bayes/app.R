library(shiny)
library(ggplot2)
library(shinythemes)
library(scales)
library(shinyMobile)
library(eulerr)


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
                
                # ui ###----
                # Application title
                titlePanel("Bayes' diagnostic test aid with Euler plot"),
                p("Written by ", tags$a(href = "https://profiles.auckland.ac.nz/s-thornley", "Dr Simon Thornley", target = "_blank")),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("prevalence",
                                paste0("Prevalence (%): ",
                                       "\nBackground disease risk."),
                                min = 0.01,
                                max = 100,
                                value = 20),
                    sliderInput("sensitivity",
                                HTML(paste0("Sensitivity (%):",
                                       "\n", "<i>P</i>(test + | Disease)")),
                                min = 1,
                                max = 100,
                                value = 80),
                    sliderInput("specificity",
                                HTML(paste0("Specificity (%):\n",
                                       "\n<i>P</i>(test - | No disease)")),
                                min = 1,
                                max = 100,
                                value = 80), 
                    
                    radioButtons("preset", label = "Scenario:", 
                                 choices = c("User input" = "1",
                                             "?Pulmonary embolus in hospitalised adult patient; \n\nD-dimer" = "2",
                                             "?Colorectal cancer from population faecal occult blood test (80 years or older)" = "3"), 
                                 selected = "1"),
                    
                    radioButtons("pos_neg", label = "Test positive or negative?", 
                                 choices = c("Positive" = "1",
                                             "Negative" = "2"), 
                                 selected = "1"),
                    
                    br(), br(),
                    sliderInput("font_size", 
                                "Euler plot: font size",
                                min = 0.5,
                                max = 3,
                                value = 1.5),
                    sliderInput("transparency", 
                                "Euler plot: adjust transparency",
                                min = 0,
                                max = 1,
                                value = 0.5)
                  ),
                  
                  
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    #img(src='numbers.jpg', align='center'),
                    #br(), br(),
                    p("This app helps to visualise simple Bayesian calculations 
                    of the accuracy of diagnostic tests, based on the prevalence
                     of the ", strong("disease"),"  and ", strong("sensitivity"),
                      " and ", strong("specificity")," of the test
                      (as compared to a gold standard diagnostic technique).",
                      br(), 
                      br(),
                      "On the left is the circle that represents those with ", 
                      strong("disease,"),"and on the right are those that are ",
                      strong("disease free."),"In the middle
                      are those who are ",
                      strong("test positive."),
                      br(), 
                      br(),
                      "Either provide your own values of disease ",
                      strong("prevalence", .noWS = "after"), ",",
                      strong("sensitivity"), " and ", strong("specificity"), 
                      " to estimate the post-test probability of the disease
        if the test is either negative or positive. 
        There are also a number of scenarios under construction. 
        See the ", strong("Scenario", .noWS = "after"), " radio button on the left of the screen."),
                    br(),
                    htmlOutput("source_info"),
                    br(),
                    plotOutput("euler"),
                    br(),
                    p(style="text-align: center;", 
                      "Risk of disease if ", span("test-positive (%): ", strong(textOutput("post_prob_pos", 
                                                                                           inline = TRUE)), 
                                                  inline = TRUE, style = "color:#00BFC4"), inline = TRUE),
                    p(style = "text-align: center;", 
                      "Risk of disease if ", span("test-negative (%): ", strong(textOutput("post_prob_neg",
                                                                                           inline = TRUE)), 
                                                  inline = TRUE, style = "color:red"), inline = TRUE),
                    
                    br()
                    
                  ))
                
)


server <- function(input, output, session) {
  
  
  sens <- reactive(input$sensitivity/100)
  spec <- reactive(input$specificity/100)
  prev <- reactive(input$prevalence/100)
  
  sns = 0.8
  spc = 0.8
  prv = 0.2
  
  
  observeEvent(input$preset, {
    #font_size <- input$font_size
    #alpha <- input$transparency
    
    if(input$preset == "2") {
      sns = .90
      spc = .30
      prv = .08
    } 
    if(input$preset == "3") {
      sns = .86
      spc = .85
      prv = 0.005
    } 
    
    updateSliderInput( session ,
                       "sensitivity", 
                       value = sns*100)
    updateSliderInput( session ,
                       "specificity", 
                       value = spc*100)
    updateSliderInput( session,
                       "prevalence", 
                       value = prv*100)
    # updateSliderInput( session,
    #                    "font_size", 
    #                    value = font_size)
    # updateSliderInput( session,
    #                    "transparency", 
    #                    value = alpha)
  })
  
  ## output plot
  output$euler <- renderPlot({
    # generate bins based on input$bins from ui.R
    # make 2x2 table
    Prevalent_cases <- (prev()) * 10000
    Prevalent_non_cases <- (1-prev()) * 10000
    true_pos <- Prevalent_cases * sens()
    false_pos <- Prevalent_non_cases * (1-spec())
    true_neg <- Prevalent_non_cases * spec()
    false_neg <- Prevalent_cases * (1-sens())
    
    if(input$pos_neg == "1"){
    
    fit1 <- euler(c(
      "Disease" = false_neg,
      "Test positive&No disease" = false_pos,
      "Disease&Test positive" = true_pos,
      "No disease" = true_neg
    ))
    } else {
      fit1 <- euler(c(
        "Disease" = true_pos,
        "Test negative&No disease" = true_neg,
        "Disease&Test negative" = false_neg,
        "No disease" = false_pos
      ))
    }
    
    
    
    plot(fit1, 
         shape = "ellipse",
         labels = list(fontfamily = "serif", cex = input$font_size),
         quantities = list(type = "percent", cex = input$font_size),
         fills = list(fill = c("orange", "#00BFC4", "green"), 
                      alpha = input$transparency),
         legend = list(side = "right", cex = input$font_size),
         edges = list(lty = c(1, 3, 1), 
              lwd = (3))
    )
    
    # p <- nomogrammer(Prevalence = input$prevalence/100, Sens = input$sensitivity/100, 
    #                  Spec = input$specificity/100,
    #                  Detail = TRUE, NullLine = TRUE,
    #                  LabelSize = 20/5)
    # p 
  })
  observe({
    if(input$preset == "1"){
      output$source_info <- renderUI(""
      )
    }
    if(input$preset == "2"){
      output$source_info <- renderUI(
        
        p(h4("D-dimer for the diagnosis of pulmonary embolism"),"The source document is ", 
          tags$a(href="https://www.nature.com/articles/s41598-022-16515-6", "here.",
                 target="_blank"), "This was an Israeli retrospective cohort study of hospitalised patients with D-dimers requested in hospital between 2014 to 2019.",
          " The total cohort was 354 patients, 56% of whom underwent definitive diagnostic imaging. 
      95.5% of cases were from medical wards, with 4.5% from surgical wards.",
          " The median age was 69 years.", "The study ", strong("sensitivity"), " 
      was 90% (95% CI: 73 to 98%) and the ", strong("specificity"), " was 30% (95% CI: 24 to 37%).",
          " This means that a ", strong("positive test", style = "color:#00BFC4"), " increases the odds 
      of a pulmonary embolus
      by ", strong("30%", style = "color:#00BFC4"), " and a ", strong("negative test", style = "color:red")," reduces 
      the odds of a diagnosis of pulmonary embolus by ", strong("67%.", style = "color:red"))
      )
    } 
    if(input$preset == "3"){
      output$source_info <- renderUI(
        p(h4("Faecal occult blood for diagnosis of colorectal cancer"),"The source document is ", 
          tags$a(href="https://ar.iiarjournals.org/content/anticanres/40/7/3591.full.pdf", "here.",
                 target="_blank"), "This is a meta-analysis of traditional and immunochemical faecal occult blood testing for colorectal screening.",
          " The results are given for the superior immunochemical test.",
          " The pooled ", strong("sensitivity"), " was 86% (95% CI: 78 to 93%) and the ", strong("specificity"), " was 85% (95% CI: 81 to 88%).",
          " This means that a ", strong("positive test", style = "color:#00BFC4"), " increases the odds 
      of colorectal cancer by ", strong("5.7 times", style = "color:#00BFC4"), " and a ", strong("negative test", style = "color:red"),
          " reduces the odds of a diagnosis by ", strong("6.1 times", style = "color:red", .noWS = "after"), ". The prevalence of disease is age dependent and according to this ",
          tags$a(href="https://www.hqsc.govt.nz/assets/resources/Health-Quality-Evaluation/Atlas/BowelCancerSF/atlas.html", "source", target="_blank")," the 
      annual incidence, which we will use for prevalence, is about ", strong("0.5%") ," in people aged 80 years and over. ",
          "A prevalence of ", strong("1%")," is used for illustration.")
      )
      
    }
  })
  
  
  
  output$post_prob_pos <- renderText({
    prior_prob  <- input$prevalence/100
    prior_odds  <- prior_prob/(1-prior_prob)
    sensitivity <- input$sensitivity/100
    specificity <- input$specificity/100
    PLR <- sensitivity/(1-specificity)
    NLR <- (1-sensitivity)/specificity
    post_odds_pos  <- prior_odds * PLR
    post_prob_pos  <- post_odds_pos/(1+post_odds_pos)
    (post_prob_pos*100) |> formatC(digits = 1, format = "f")
  })                          
  output$post_prob_neg <- renderText({
    prior_prob  <- input$prevalence/100
    prior_odds  <- prior_prob/(1-prior_prob)
    sensitivity <- input$sensitivity/100
    specificity <- input$specificity/100
    PLR <- sensitivity/(1-specificity)
    NLR <- (1-sensitivity)/specificity
    post_odds_neg  <- prior_odds * NLR
    post_prob_neg  <- post_odds_neg/(1+post_odds_neg)
    (post_prob_neg*100) |> formatC(digits = 1, format = "f")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
