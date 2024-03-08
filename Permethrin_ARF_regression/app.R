# Must call r script app.R for deployment.
 #rm(list = ls())

# setup libraries.
#install.packages("DT")
library(shiny)
library(visreg)
library(rvest)
library(magrittr)
library(DT) # Data table
library(regplot)
library(shinythemes)
library(plotly)
library(reshape2)
library(epiDisplay)
library(rms)
library(car)



two_dp <- function(x){
  formatC(x, digits = 2, format = "f")
}

zero_dp <- function(x){
  formatC(x, digits = 0, format = "f")
}


##----  plot regression surface.----
load('data/ARF_permethrin.rdata')

three_d_plot_with_regression_surface <- function(df = for_shiny, reg_model = "linear",
                                                 covars = c("Pacific_proportion", "rate_tot_disp"), extra_info = "cau2013",
                                                 outcome = "rate_tot_arf", adjusted_or_crude = "crude",
                                                 dimension = c(10, 10), opacity_surface = 0.75, get_model = FALSE){

  run_model <- function(df1 = df, reg_model1 = reg_model, covars1 = covars, outcome1 = outcome, adjust = adjusted_or_crude){ 
  if (reg_model1 == "linear outcome" & adjust == "crude (Permethrin \u2192 ARF)"){
    
    model3 <- lm(as.formula( paste0(outcome1, "~", covars1[2])), data = df1, 
                 na.action = na.exclude)
  }
    if (reg_model1 == "linear outcome" & adjust == "crude (Pacific \u2192 ARF)"){
      
      model3 <- lm(as.formula( paste0(outcome1, "~", covars1[1])), data = df1, 
                   na.action = na.exclude)
    }
    
     if (reg_model1 == "linear outcome" & adjust =="adjusted"){
    #browser()
    model3 <- lm(as.formula(paste0(outcome1, "~", covars1[1],  '+', covars1[2])), data = df1, 
                 na.action = na.exclude)
     }
    
    if (reg_model1 == "linear outcome with squared terms" & adjust == "crude (Permethrin \u2192 ARF)"){
      
      model3 <- lm(as.formula( paste0(outcome1, "~", "I(", covars1[2], "^2)")), data = df1, 
                   na.action = na.exclude)
    }
    if (reg_model1 == "linear outcome with squared terms" & adjust == "crude (Pacific \u2192 ARF)"){
      
      model3 <- lm(as.formula( paste0(outcome1, "~ I(", covars1[1], "^2)")), data = df1, 
                   na.action = na.exclude)
    }
    
    if (reg_model1 == "linear outcome with squared terms" & adjust =="adjusted"){
      #browser()
      model3 <- lm(as.formula(paste0(outcome1, "~ I(", covars1[1],"^2) + I(", covars1[2], "^2)")), data = df1, 
                   na.action = na.exclude)
    }
    
    
    
    if (reg_model1 == "linear outcome with quadratic terms" & adjust == "crude (Permethrin \u2192 ARF)"){
      model3 <- lm(as.formula(paste0(outcome1, "~", covars1[2],' +', 
                                      'I(', covars1[2], '^ 2)')), 
                   data = df1, na.action = na.exclude)
    }
    if (reg_model1 == "linear outcome with quadratic terms" & adjust == "crude (Pacific \u2192 ARF)"){
      model3 <- lm(as.formula(paste0(outcome1, "~", covars1[1],' +', 
                                     'I(', covars1[1], '^ 2)')), 
                   data = df1, na.action = na.exclude)
    }  
    
  if (reg_model1 == "linear outcome with quadratic terms" & adjust == "adjusted"){
    model3 <- lm(as.formula(paste0(outcome1, "~", covars1[1],  '+', covars1[2],' +', 
                'I(', covars1[1], '^ 2)', '+', 'I(', covars1[2], '^ 2)')), 
                data = df1, na.action = na.exclude)
  }
    
    if (reg_model1 == "Poisson with linear terms" & adjust == "adjusted"){
    model3 <- glm(as.formula(paste0(outcome1, "~", covars1[1],  '+', covars1[2])), 
                family = "poisson",
                data = df1, na.action = na.exclude)
    }
    
    if (reg_model1 == "Poisson with linear terms" & adjust == "crude (Permethrin \u2192 ARF)"){
    model3 <- glm(as.formula(paste0(outcome1, "~",  covars1[2])), 
                family = "poisson",
                data = df1, na.action = na.exclude)
    }
    
    if (reg_model1 == "Poisson with linear terms" & adjust == "crude (Pacific \u2192 ARF)"){
    model3 <- glm(as.formula(paste0(outcome1, "~",  covars1[1])), 
                family = "poisson",
                data = df1, na.action = na.exclude)
  }
    
    
    if (reg_model1 == "Poisson with quadratic terms" & adjust == "crude (Pacific \u2192 ARF)"){
    model3 <- glm(as.formula(paste0(outcome1, "~", covars1[1],' +', 
                                     'I(', covars1[1], '^ 2)')), 
                family = "poisson",
                data = df1, na.action = na.exclude)
    }
     if (reg_model1 == "Poisson with quadratic terms" & adjust == "adjusted"){
    model3 <- glm(as.formula(paste0(outcome1, "~", covars1[1],  '+', covars1[2],' +', 
                'I(', covars1[1], '^ 2)', '+', 'I(', covars1[2], '^ 2)')), 
                family = "poisson",
                data = df1, na.action = na.exclude)
    }
    
    if (reg_model1 == "Poisson with quadratic terms" & adjust == "crude (Permethrin \u2192 ARF)"){
    model3 <- glm(as.formula(paste0(outcome1, "~", covars1[2],' +', 
                                     'I(', covars1[2], '^ 2)')), 
                family = "poisson",
                data = df1, na.action = na.exclude)
    }
    
    if (reg_model1 == "Poisson with restricted cubic spline" & adjust == "crude (Pacific \u2192 ARF)"){
    model3 <- Glm(as.formula(paste0(outcome1, "~", "rcs(", covars1[1],")")), 
                family = poisson,
                data = df1, na.action = na.exclude)
    }
     if (reg_model1 == "Poisson with restricted cubic spline" & adjust == "crude (Permethrin \u2192 ARF)"){
    model3 <- Glm(as.formula(paste0(outcome1, "~",  "rcs(", covars1[2],")")), 
                family = poisson,
                data = df1, na.action = na.exclude)
    }
    
    if (reg_model1 == "Poisson with restricted cubic spline" & adjust =="adjusted" ){
    model3 <- Glm(as.formula(paste0(outcome1, "~", "rcs(", covars1[2],') + rcs(', 
                                     covars1[1], ')')), 
                family = poisson,
                data = df1, na.action = na.exclude)
  
    }
    
    
    
    return(model3)
  }
 # browser()
  
  model <- run_model()
  
  if(get_model == TRUE){
    return(model)
  }
  
  
  # model %>% print
  stopifnot(df %>%  is.data.frame)
  ## Calculate various variables required for plotting.
  if (reg_model == "Poisson with restricted cubic spline"){
    df$predicted_outcome <- predict(model, newdata = df, type = "lp") %>% exp 
  } else {
  df$predicted_outcome <- predict(model, newdata = df) %>% as.numeric
  }
  #df %>% str %>% print
  df <- na.exclude(df)
  
  df$predicted_minus_observed_outcome <- df$predicted_outcome - df[, outcome] 
  
  df$predicted_minus_observed_outcome %>% str
  
  df$opacity <- (df[, outcome] - df$predicted_outcome )^2/ max((df[, outcome] - df$predicted_outcome )^2 %>% abs)
  
  df$predicted_outcome %>% zero_dp
  
  df %<>% data.frame
  
  
  #print("df is now:")
  #str(df) %>% print
  
  n_points <- 25
  graph_reso_first_covar <- (max(df[, covars[1], drop = FALSE]) - min(df[, covars[1], drop = FALSE]))/n_points
  #print("graph_reso_mileage is:")
  #print(graph_reso_mileage)
  
  graph_reso_second_covar <- (max(df[, covars[2], drop = FALSE]) - min(df[, covars[2], drop = FALSE]))/n_points
  #print("graph_reso_year is:")
  #print(graph_reso_year)
  #Setup Axis
  axis_x <- seq(min(df[, covars[1], drop = FALSE]), max(df[, covars[1], drop = FALSE]), 
                by = graph_reso_first_covar)
  axis_y <- seq(min(df[, covars[2], drop = FALSE]), max(df[, covars[2], drop = FALSE]), 
                by = graph_reso_second_covar)
  model_surface <- expand.grid(x = axis_x, y = axis_y, KEEP.OUT.ATTRS = F)
  names(model_surface) <- covars
  
  if(reg_model == "linear outcome" | reg_model == "linear outcome with quadratic terms" | 
     reg_model == "linear outcome with squared terms" ){
  model_surface$outcome <- predict.lm(model, newdata = model_surface)
  } 
  
  if (reg_model == "Poisson with linear terms" | reg_model == "Poisson with quadratic terms"){
    model_surface$outcome <- predict.glm(model, newdata = model_surface, type = "response")
  }
  
  if (reg_model == "Poisson with restricted cubic spline"){
    model_surface$outcome <- predict(model, newdata = model_surface, type = "lp") %>% exp
  }
  
  print("model surface outcome is:")
  print(str(model_surface$outcome))
  
  model_surface <- acast(model_surface, as.formula(paste0(covars[2], "~", covars[1])),
                         value.var = "outcome") #y ~ x
  surf <- cbind(axis_x, axis_y, model_surface) %>% data.frame
  
  # Create lists for axis properties
  f1 <- list(
    family = "Arial, sans-serif",
    size = 18,
    color = "black")
  
  f2 <- list(
    family = "Old Standard TT, serif",
    size = 14,
    color = "#ff9999")
  
  xaxis <- list(
    titlefont = f1,
    tickfont = f2,
    showgrid = TRUE,
    title = covars[1]
                )
  
  yaxis <- list(
    titlefont = f1,
    tickfont = f2,
    showgrid = TRUE,
    title =  covars[2]
                )
  
  
  zaxis <- list(
    titlefont = f1,
    tickfont = f2,
    showgrid = TRUE,
    title = outcome
                )
  
  
  scene = list(
    xaxis = xaxis,
    yaxis = yaxis,
    zaxis = zaxis
  )
  
 # names(df) <- 
  
  #- Plot 
  plot_3d <- plot_ly( data = df, 
                     x = ~df[, covars[1]], 
                     y = ~df[, covars[2]],
                     z = ~df[, outcome],
                     type = "scatter3d",
                     name = "ARF_permethrin",
                     marker = list(size = 5, 
                                   color = df$opacity, 
                                   line = list(
                                     color = 'black',
                                     width = 1)),
                     color = 'rgba(255, 182, 193, .9)',
                     width = (0.6*as.numeric(dimension[1])), 
                     height = 0.7*as.numeric(dimension[2]),
                     hoverinfo = 'text',
                     hoverlabel = list(bgcolor = "black", font = list(color = "white")),
                     text = ~paste('Pacific proportion', df[, covars[1]] %>% two_dp, 
                                   '<br>Permethrin rate:', df[, covars[2]], 
                                   '<br>Rheumatic fever rate:', df[, outcome],
                                   '<br>Suburb: ', df[, extra_info] )) %>% 
  layout(title = "", scene = list(
    xaxis = list(title = "Pacific proportion"),
    yaxis = list(title = "Permethrin rate"),
    zaxis = list(title = "Acute rheumatic fever rate"))) %>% suppressWarnings
  
  plot_3d <- add_trace(p = plot_3d,
                      data = surf,
                      z = ~model_surface,
                      x = ~axis_x,
                      y = ~axis_y,
                      opacity = opacity_surface,
                      type = "surface",
                      name = "regression plane",
                      showlegend = F,
                      hoverinfo = 'all',
                      text = ~paste('Pacific proportion', df[, covars[1]] %>% two_dp, 
                                    '<br>Permethrin rate:', df[, covars[2]], 
                                    '<br>Rheumatic fever rate:', df[, outcome]
                                    )
                      ) %>% suppressWarnings
  plot_3d
  }





#---- Define UI for app  ----
ui <- fluidPage(theme = shinytheme("cyborg"), ## provides theme for shiny app...
                # this provides dimensions for plotly output.
                
      #--- This styles validation error messages... --- 
      
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
  tabPanel("Learn about regression", 
  # App title ----
  headerPanel("Spatial characteristics associated with acute rheumatic fever incidence"),
  sidebarLayout(
    sidebarPanel(
      selectInput('reg_model', 'Regression Model', c("linear outcome",
                                                     "linear outcome with squared terms",
                                                     "linear outcome with quadratic terms",
                                                      "Poisson with linear terms", "Poisson with quadratic terms",
                                                        "Poisson with restricted cubic spline")),
      selectInput('adjust', 'Crude or adjusted model?', c("crude (Permethrin \u2192 ARF)",
                                                          "crude (Pacific \u2192 ARF)", "adjusted"))),
  # Sidebar layout with input and output definitions ----
  # 3d plot tab----
  mainPanel("This plot illustrates the relationship between acute rheumatic fever incidence, 
            proportion of people who identify
           as Pacific ethnicity and the rate of permethrin prescribing for each census area unit. 
            Rates are per 100,000 people per year.",
           br(),
           br(),
           plotlyOutput("three_d_plot", width = "100%", height = "100%"))
  
)),
  tabPanel("Diagnostic Plots",
           mainPanel(br(),
                    br(),
                    "Here, we have an added variable plot, in which the partial relationship between
                    the response and a regressor, is adjusted for all other regressors.",
                    br(),
                    br(),
                    plotOutput("addedVariablePlot")
                  
                     )),
tabPanel("Regression nomogram", 
         headerPanel("Visualise a regression model as a nomogram"),
         mainPanel("This is a nomogram of the regression model, from Roger Marshall's ", tags$i("regplot"), " package.",
                   "\n The independent variables (permethrin [here: 'rate_tot_disp'] and Pacific proportion) contribute points that correspond to a probability
                                       of the outcome (acute rheumatic fever incidence). Density plots indicate the distribution of each independent variable. 
                                       Asterisks indicate statistical significance.",
                   br(),
                   br(),
                   plotOutput("regplot_logistic", width = "100%", height = "800px")))


))






# Define server logic ----
server <- function(input, output) {
 
   
#--- Render 3d plot ----
  
  output$three_d_plot <- renderPlotly({
    
    three_d_plot_with_regression_surface(df = for_shiny, reg_model = input$reg_model,
                                         covars = c("Pacific_proportion", "rate_tot_disp"), 
                                         outcome = "rate_tot_arf", adjusted_or_crude = input$adjust,
                                         dimension = input$dimension)
  })
    
  output$addedVariablePlot <- renderPlot({
      
      model <-  three_d_plot_with_regression_surface(df = for_shiny, reg_model = input$reg_model,
                                                     covars = c("Pacific_proportion", "rate_tot_disp"), 
                                                     outcome = "rate_tot_arf", adjusted_or_crude = input$adjust,
                                                     dimension = input$dimension, get_model = TRUE)
      
      avPlots(model)
      
    })
 
  output$regplot_logistic <- renderPlot({
    model <- three_d_plot_with_regression_surface(df = for_shiny, reg_model = input$reg_model,
                                                  covars = c("Pacific_proportion", "rate_tot_disp"), 
                                                  outcome = "rate_tot_arf", adjusted_or_crude = input$adjust,
                                                  dimension = input$dimension, get_model = TRUE)
    regplot(model, clickable = FALSE)
    
  })
  



}

shinyApp(ui = ui, server = server)



