# Load required libraries
library(shiny)
library(readxl)
library(forecast)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(rio)
library(tscount)
library(mathjaxr)
library(markdown)
library(later)
# install.packages(c("future", "future.apply", "promises"))
library(future)
library(future.apply)
library(promises)
library(shinyjs)
# install.packages("waiter")
library(waiter)
# Set up a parallel plan. On shiny.io, multisession is usually supported.
plan(multisession)
# Preload the Excel file
data_path <- "./data/stmf.xlsx"  # Adjust the file path as needed
all_sheets <- excel_sheets(data_path)
all_sheets <- all_sheets[-1]
Country_code <- import("./data/Country.xlsx")



# Define the UI
ui <- fluidPage(
  use_waiter(),  # Initialize waiter,
  # Custom CSS to style widgets
  tags$head(
    tags$style(HTML("
      /* Style for selectInput with blue label - country selector */
      .blue-text .selectize-input {
        color: blue !important;
      }
      .blue-text .selectize-dropdown-content {
        color: blue !important;
      }
      .blue-text .selectize-dropdown .active {
        background: rgba(0, 0, 255, 0.1) !important;
      }
      
      /* Style for selectInput with green label - age category */
      .green-text .selectize-input {
        color: green !important;
      }
      .green-text .selectize-dropdown-content {
        color: green !important;
      }
      .green-text .selectize-dropdown .active {
        background: rgba(0, 128, 0, 0.1) !important;
      }
      
      /* Style for selectInput with brown label - data type */
      .brown-text .selectize-input {
        color: brown !important;
      }
      .brown-text .selectize-dropdown-content {
        color: brown !important;
      }
      .brown-text .selectize-dropdown .active {
        background: rgba(165, 42, 42, 0.1) !important;
      }
      
      /* Style for selectInput with navy label - change point method */
      .navy-text .selectize-input {
        color: navy !important;
      }
      .navy-text .selectize-dropdown-content {
        color: navy !important;
      }
      .navy-text .selectize-dropdown .active {
        background: rgba(0, 0, 128, 0.1) !important;
      }
      
      /* Base slider styles - lighter bars with dark text */
      .irs-bar, .irs-bar-edge, .irs-line {
        background: #e0e0e0 !important;
        border-color: #b0b0b0 !important;
      }
      .irs-slider {
        background: #606060 !important;
      }
      
      /* Slider text colors */
      .irs-from, .irs-to, .irs-single, .irs-min, .irs-max {
        color: maroon !important;
        background: #f8f8f8 !important;
      }
      
      /* Olive slider text */
      .olive-text .irs-from, 
      .olive-text .irs-to, 
      .olive-text .irs-single, 
      .olive-text .irs-min, 
      .olive-text .irs-max {
        color: olive !important;
        background: #f8f8f8 !important;
      }
      
      /* Red slider text */
      .red-text .irs-from, 
      .red-text .irs-to, 
      .red-text .irs-single, 
      .red-text .irs-min, 
      .red-text .irs-max {
        color: red !important;
        background: #f8f8f8 !important;
      }
      
      /* Make dropdown options match the text color */
      .selectize-dropdown-content div {
        color: inherit !important;
      }
      
      /* Hover effect for dropdown options */
      .selectize-dropdown .option:hover {
        background-color: #f5f5f5 !important;
      }
    "))
  ),
  useShinyjs(),  # Initialize shinyjs
  tags$head(
    tags$style(HTML("
      /* Centered spinner */
      #loading-spinner {
        position: fixed;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        z-index: 9999;
        display: none;
      }
    "))
  ),
  
  titlePanel("Analysis of all-cause mortality trends with change point detection"),
  withMathJax(),
  includeMarkdown("shiny_intro.qmd"),
  br(),
  sidebarLayout(
    sidebarPanel(
      selectInput("sheet", span("Select country", style="color:blue"), choices = Country_code$Country) |> 
        tagAppendAttributes(class = "blue-text"),
      selectInput("age_category", span("Select age category", style="color:green"), choices = NULL) |> 
        tagAppendAttributes(class = "green-text"),
      selectInput("data_type", span("Select data type", style="color:brown"),
                  choices = c("Count" = "count", "Rate" = "rate")) |> 
        tagAppendAttributes(class = "brown-text"),
      sliderInput("t_window", span("Trend-cycle window (weeks)", style="color:maroon"), 
                  min = 5, max = 52, step = 2, value = 52),
      sliderInput("s_window", span("Seasonal-cycle window (years)", style="color:maroon"), 
                  min = 3, max = 25, step = 1, value = 25),
      selectInput("cp_method", span("Change Point Detection", style="color:navy"), 
                  choices = c("None", "tscount (interv_detect)")) |> 
        tagAppendAttributes(class = "navy-text"),
      
      actionButton("optimize_delta", "Optimize Intervention", 
                   style = "color: white; background-color: olive; border: none; padding: 8px 16px; text-align: center;"),
      
      sliderInput("delta", span("Intervention to detect\n(0 = spike,\n0.8 increase and decay,\n1 = step increase)", style="color:olive"),
                  min = 0, max = 1, step = 0.01, value = 0.8) |> 
        tagAppendAttributes(class = "olive-text"),
      
      uiOutput("half_life_info") |> 
        tagAppendAttributes(style = "margin-top: 10px; font-weight: bold; color: olive;"),
      
      sliderInput("num_cps", span("Number of candidate change points to display:", style="color:red"),
                  min = 1, max = 20, value = 4, step = 1) |> 
        tagAppendAttributes(class = "red-text")
    ),
    mainPanel(
      plotOutput("time_series_plot"),
      plotOutput("interventionPlot", height = "200px",
                 hover = hoverOpts(id = "plot_hover", delay = 100)) |> 
        tagAppendAttributes(style = "margin-top: 10px; border: 1px solid #ddd; padding: 10px;"),
      conditionalPanel(
        condition = "input.cp_method == 'tscount (interv_detect)'",
        h3("Diagnostic plots and change point detection output"),
        htmlOutput("cp_results"),
        h4("Magnitude of intervention change test statistic"),
        plotOutput("int_plot"),
        h4("Count model: observed values vs. model predicted"),
        plotOutput("obs_vs_pred", height = "800px"),
        h4("Count model: Probability integral transform plot"),
        p("Should be uniform distribution if good model fit."),
        plotOutput("fit_plot_pit"),
        h4("Count model: Other diagnostic plots"),
        plotOutput("fit_plot")
      )
    )
  )
)
# Define the server logic (unchanged from your code)
server <- function(input, output, session) {
  
  # Force data_type to "count" if any change point detection method is selected.
  observeEvent(input$cp_method, {
    if (input$cp_method != "None") {
      updateSelectInput(session, "data_type", choices = c("Count" = "count"), selected = "count")
    } else {
      updateSelectInput(session, "data_type", choices = c("Count" = "count", "Rate" = "rate"))
    }
  })
  
  # Load selected sheet's data
  data <- reactive({
    req(input$sheet)
    df <- import(data_path, sheet = Country_code[Country_code$Country == input$sheet, "Code"], skip = 2)
    names(df)[5:10] <- paste0("count_", gsub("\\..*", "", names(df)[5:10]))
    names(df)[11:16] <- paste0("rate_", gsub("\\..*", "", names(df)[11:16]))
    df <- df[df$Sex == "b", ]
    return(df)
  })
  
  # Update age category choices based on data
  observeEvent(data(), {
    req(data())
    cols <- names(data())
    age_categories <- unique(str_extract(cols, "0-14|15-64|65-74|75-84|85\\+"))
    age_categories <- age_categories[-1]
    age_categories <- c("Total", age_categories)
    updateSelectInput(session, "age_category", choices = age_categories)
  })
  
  # Change point detection using tscount with interv_covariate (server code unchanged)
  detect_changepoints <- reactive({
    req(data(), input$age_category, input$data_type, input$cp_method)
    column_name <- paste0(input$data_type, "_", input$age_category)
    if (!column_name %in% names(data())) {
      showNotification("Invalid selection. Column not found.", type = "error")
      return(NULL)
    }
    start_date <- data()$Year[1] + (data()$Week[1] - 1) / 52
    time_series <- ts(data()[[column_name]], frequency = 52, start = start_date)
    
    if (input$cp_method == "tscount (interv_detect)") {
      regressors <- cbind(linearTrend = seq(along = time_series) / 52)
      fit <- tsglm(ts = time_series,
                   xreg = regressors,
                   model = list(past_obs = c(1, 52)),
                   distr = "nbinom", link = "log",
                   info = "score")
      interv_result <- interv_detect(fit, delta = input$delta, est_interv = TRUE)
      
      tau_scores <- as.vector(interv_result$test_statistic_tau)
      top_n <- input$num_cps
      top_indices <- order(tau_scores, decreasing = TRUE)[1:top_n]
      
      candidate_numeric <- time(time_series)[top_indices]
      candidate_dates <- date_decimal(candidate_numeric)
      
      # Compute intervention effects for each candidate using interv_covariate()
      effects <- numeric(length(candidate_numeric))
      time_vec <- time(time_series)
      for (i in seq_along(candidate_numeric)) {
        tau_i <- top_indices[i]
        if (tau_i == 1 || tau_i == length(time_series)) {
          effects[i] <- NA
        } else {
          intervention_indicator <- interv_covariate(n = length(time_series), tau = tau_i, delta = input$delta)
          fit_i <- tsglm(ts = time_series,
                         xreg = cbind(linearTrend = seq(along = time_series) / 52,
                                      intervention = intervention_indicator),
                         model = list(past_obs = c(1, 52)),
                         distr = "nbinom", link = "log",
                         info = "score")
          beta <- fit_i$coefficients["interv_1"]
          effects[i] <- round(100 * (exp(beta) - 1), 5)
        }
      }
      
      return(list(
        method = "tscount",
        cpts_numeric = candidate_numeric,
        cpts = candidate_dates,
        sizes = tau_scores[top_indices],
        effects = effects,
        object = interv_result,
        fit = interv_result$fit_interv,
        null_fit = interv_result$fit_H0,
        top_indices = top_indices
      ))
    } else {
      return(NULL)
    }
  })
  
  # (Other server outputs remain unchanged)
  output$cp_results <- renderPrint({
    req(data(), input$age_category, input$data_type, input$cp_method)
    column_name <- paste0(input$data_type, "_", input$age_category)
    if (!column_name %in% names(data()))
      return("Invalid selection. Column not found.")
    
    cp_result <- detect_changepoints()
    
    if (is.null(cp_result)) {
      return(cat("No change point detection method selected."))
    } else if (cp_result$method == "tscount") {
      out_str <- paste0("Top ", input$num_cps, " candidate change points detected by tscount (interv_detect):", br())
      for (i in 1:length(cp_result$cpts)) {
        date_str <- format(cp_result$cpts[i], format = "%B %d %Y")
        score_str <- sub("\\.?0+$", "", sprintf("%.2f", cp_result$sizes[i]))
        effect_str <- formatC(cp_result$effects[i], digits = 2, format = "g", flag = "#")
        direction <- ifelse(cp_result$effects[i] < 0, " decrease", " increase")
        out_str <- paste0(out_str, i, ". ", date_str, " (test statistic: ", score_str, 
                          ", intervention effect: ", effect_str, "%", direction, ")", br())
      }
      cat(out_str)
    }
  })
  
  output$time_series_plot <- renderPlot({
    req(data(), input$age_category, input$data_type, input$cp_method)
    column_name <- paste0(input$data_type, "_", input$age_category)
    if (!column_name %in% names(data())) {
      showNotification("Invalid selection. Column not found.", type = "error")
      return(NULL)
    }
    start_date <- data()$Year[1] + (data()$Week[1] - 1) / 52
    time_series <- ts(data()[[column_name]], frequency = 52, start = start_date)
    decomposition <- stl(time_series, s.window = input$s_window, t.window = input$t_window)
    
    cp_result <- detect_changepoints()
    
    p <- autoplot(decomposition) +
      ggtitle(paste0("Seasonal trend decomposition of weekly ", input$data_type, "s of deaths in ",
                     input$sheet, "\n", ifelse(input$age_category == "Total", "All ages", input$age_category),
                     ifelse(input$age_category == "Total", "", " years"))) +
      theme(plot.title = element_text(face = "bold")) +
      geom_vline(xintercept = c(2021, 2022), linetype = "dotted", size = 1, colour = "purple") +
      theme_gray(base_size = 14)
    
    if (!is.null(cp_result) && cp_result$method %in% c("tscount", "bcp")) {
      for (i in seq_along(cp_result$cpts_numeric)) {
        if (i == 1) {
          col_val <- scales::alpha("red", 1)
        } else {
          alpha_val <- max(1 - 0.1 * (i - 1), 0.3)
          col_val <- scales::alpha("#CA5900", alpha_val)
        }
        p <- p + geom_vline(xintercept = cp_result$cpts_numeric[i], 
                            linetype = "dashed", 
                            color = col_val,
                            linewidth = 1)
      }
    }
    print(p)
  })
  
  output$int_plot <- renderPlot({
    req(data(), input$age_category, input$data_type, input$cp_method)
    column_name <- paste0(input$data_type, "_", input$age_category)
    if (!column_name %in% names(data()))
      return(NULL)
    start_date <- data()$Year[1] + (data()$Week[1] - 1) / 52
    time_series <- ts(data()[[column_name]], frequency = 52, start = start_date)
    cp_result <- detect_changepoints()
    if (!is.null(cp_result)) {
      plot(cp_result$object)
    } else {
      return(NULL)
    }
  })
  
  output$fit_plot <- renderPlot({
    req(data(), input$age_category, input$data_type, input$cp_method)
    column_name <- paste0(input$data_type, "_", input$age_category)
    if (!column_name %in% names(data()))
      return(NULL)
    start_date <- data()$Year[1] + (data()$Week[1] - 1) / 52
    time_series <- ts(data()[[column_name]], frequency = 52, start = start_date)
    cp_result <- detect_changepoints()
    if (!is.null(cp_result)) {
      layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow = TRUE),
             widths = c(2, 2), heights = rep(5, 3))
      plot(cp_result$fit$residuals, main = "")
      hist(cp_result$fit$residuals, main = "")
      acf(cp_result$fit$residuals, main = "")
      pacf(cp_result$fit$residuals, main = "")
      plot(as.vector(cp_result$fit$response), 
           as.vector(cp_result$fit$fitted.values),
           xlab = "response", ylab = "fitted")
      par(mfrow = c(1, 1))
    } else {
      return(NULL)
    }
  })
  
  output$fit_plot_pit <- renderPlot({
    req(data(), input$age_category, input$data_type, input$cp_method)
    column_name <- paste0(input$data_type, "_", input$age_category)
    if (!column_name %in% names(data()))
      return(NULL)
    start_date <- data()$Year[1] + (data()$Week[1] - 1) / 52
    time_series <- ts(data()[[column_name]], frequency = 52, start = start_date)
    cp_result <- detect_changepoints()
    if (!is.null(cp_result)) {
      pit(cp_result$fit, ylim = c(0, 1.5), main = "PIT Poisson")
    } else {
      return(NULL)
    }
  })
  
  output$obs_vs_pred <- renderPlot({
    req(data(), input$age_category, input$data_type, input$cp_method)
    column_name <- paste0(input$data_type, "_", input$age_category)
    if (!column_name %in% names(data()))
      return(NULL)
    start_date <- data()$Year[1] + (data()$Week[1] - 1) / 52
    time_series <- ts(data()[[column_name]], frequency = 52, start = start_date)
    cp_result <- detect_changepoints()
    if (!is.null(cp_result)) {
      plot(cp_result$fit$response,
           ylab = "weekly count (deaths)",
           xlab = "year",
           main = "Intervention model predictions (blue) vs. observed (black)")
      lines(cp_result$fit$fitted.values, col = "blue", lty = 3)
      legend("bottomright", legend = c("observed", "model"),
             col = c("black", "blue"), lty = c(1, 3), cex = 0.8)
    } else {
      return(NULL)
    }
  })
  
  ## Optimise delta code----
  # Function to find optimal delta
  
  # --- Optimization function without its own withProgress wrapper ---
  # --- Parallel optimization function ---
  find_optimal_delta_parallel <- function() {
    current_data <- isolate(data())
    current_age <- isolate(input$age_category)
    current_cp_method <- isolate(input$cp_method)
    current_data_type <- isolate(input$data_type)
    
    if (is.null(current_data) || is.null(current_age) ||
        is.null(current_cp_method) || is.null(current_data_type)) {
      showNotification("Missing input values for optimization.", type = "error")
      return(NULL)
    }
    
    if (current_cp_method != "tscount (interv_detect)") {
      showNotification("Change point method must be 'tscount (interv_detect)'.", type = "error")
      return(NULL)
    }
    
    column_name <- paste0(current_data_type, "_", current_age)
    if (!(column_name %in% names(current_data))) {
      showNotification("Invalid selection. Column not found.", type = "error")
      return(NULL)
    }
    
    start_date <- current_data$Year[1] + (current_data$Week[1] - 1) / 52
    time_series <- ts(round(current_data[[column_name]]), frequency = 52, start = start_date)
    regressors <- cbind(linearTrend = seq(along = time_series) / 52)
    
    delta_values <- seq(0, 1, by = 0.05)
    
    results_list <- future_lapply(delta_values, function(delta) {
      tryCatch({
        fit <- suppressWarnings(
          tsglm(ts = time_series,
                xreg = regressors,
                model = list(past_obs = c(1, 52)),
                distr = "nbinom", link = "log",
                info = "score")
        )
        if (is.null(fit)) return(NULL)
        interv_result <- suppressWarnings(
          interv_detect(fit, delta = delta, est_interv = TRUE)
        )
        if (is.null(interv_result)) return(NULL)
        if (interv_result$tau_max == 1 || interv_result$tau_max == length(time_series)) {
          message("Skipping delta ", delta, " because tau_max is at the boundary.")
          return(NULL)
        }
        current_aic <- tryCatch({
          intervention_indicator <- interv_covariate(n = length(time_series),
                                                     tau = interv_result$tau_max, 
                                                     delta = delta)
          fit_i <- tsglm(ts = time_series,
                         xreg = cbind(linearTrend = seq(along = time_series) / 52,
                                      intervention = intervention_indicator),
                         model = list(past_obs = c(1, 52)),
                         distr = "nbinom", link = "log",
                         info = "score")
          aic_val <- AIC(fit_i)
          if (!is.finite(aic_val)) NA_real_ else aic_val
        }, error = function(e) NA_real_)
        
        list(delta = delta, aic = current_aic, success = is.finite(current_aic))
      }, error = function(e) {
        message("Full procedure failed with delta = ", delta, ": ", e$message)
        NULL
      })
    })
    
    valid_list <- lapply(results_list, function(x) {
      if (!is.null(x)) {
        data.frame(delta = x$delta, aic = x$aic, success = x$success)
      } else NULL
    })
    results <- do.call(rbind, valid_list)
    
    if (is.null(results) || nrow(results) == 0) {
      showNotification("No valid delta values found. Using default delta = 0.8", type = "warning")
      return(list(best_delta = 0.8, best_aic = NA_real_))
    }
    
    best_idx <- which.min(results$aic)
    best_delta <- results$delta[best_idx]
    best_aic <- results$aic[best_idx]
    
    message(paste("Optimal delta:", best_delta, "with AIC:", best_aic))
    return(list(best_delta = best_delta, best_aic = best_aic))
  }
  
  
  # --- Observer for the Optimize Intervention button ---
  # Create a reactive value to store the optimization result
  # Create a reactive value to store the optimization result
  # --- Optimization observer with waiter spinner ---
  rv_optim <- reactiveVal(NULL)
  
  observeEvent(input$optimize_delta, {
    if (input$cp_method == "None") {
      showNotification(
        "Please set Change Point Detection to 'tscount (interv_detect)' before optimizing intervention. Then click Optimize Intervention again.",
        type = "error",
        duration = 10
      )
      return()
    }
    
    localSession <- session
    
    # Create a Waiter overlay with a spinner and text.
    w <- Waiter$new(
      html = tagList(
        spin_fading_circles(),  # built-in spinner from waiter
        h3("Optimizing...")
      ),
      color = "rgba(0,0,0,0.5)"  # semi-transparent dark overlay
    )
    w$show()
    
    # (Optional) Show an additional notification.
    progress_notif <- showNotification(
      "Optimizing intervention parameters...", 
      duration = NULL,
      closeButton = FALSE
    )
    
    if (input$cp_method != "tscount (interv_detect)") {
      updateSelectInput(localSession, "cp_method", selected = "tscount (interv_detect)")
    }
    updateSliderInput(localSession, "num_cps", value = 1)
    
    session$onFlushed(function() {
      future({
        find_optimal_delta_parallel()
      }, seed = TRUE) %...>% (function(res) {
        rv_optim(res)
        removeNotification(progress_notif)
        w$hide()
      }) %...!% (function(err) {
        isolate({
          showNotification(
            paste("Optimization error:", conditionMessage(err)),
            type = "error"
          )
        })
        removeNotification(progress_notif)
        w$hide()
      })
    }, once = TRUE)
  })
  
  # When the optimization result is available, update the delta slider and show notifications.
  observeEvent(rv_optim(), {
    res <- rv_optim()
    if (!is.null(res)) {
      updateSliderInput(session, "delta", value = round(res$best_delta, 2))
      if (!is.na(res$best_aic)) {
        showNotification(
          paste("Optimal delta found:", round(res$best_delta, 2), "\nAIC:", round(res$best_aic, 2)),
          type = "message",
          duration = 10
        )
      } else {
        showNotification(
          paste("Using delta:", round(res$best_delta, 2)),
          type = "warning",
          duration = 10
        )
      }
    }
  })
  
  output$half_life_info <- renderUI({
    req(input$delta)
    
    # Calculate half-life (in weeks) based on delta
    # The formula is: half_life = log(0.5)/log(delta)
    # But we need to handle edge cases:
    if (input$delta == 0) {
      half_life <- "instantaneous (spike)"
    } else if (input$delta == 1) {
      half_life <- "permanent (step change)"
    } else if (input$delta > 0 && input$delta < 1) {
      half_life_weeks <- round(log(0.5)/log(input$delta), 1)
      half_life <- paste( half_life_weeks, "weeks")
    } else {
      half_life <- "undefined"
    }
    
    # Create the display text
    tags$div(
      tags$span("Intervention half-life: "),
      tags$span(half_life, style = "font-weight: normal;")
    )
  })
  
  # Add this to your server function:
  output$interventionPlot <- renderPlot({
    req(input$delta)
    
    # Create a clean environment to avoid any naming conflicts
    local({
      weeks <- 0:20
      delta <- input$delta
      
      # Calculate effect - handle edge cases explicitly
      effect <- if (delta == 0) {
        c(1, rep(0, 20))  # Spike intervention
      } else if (delta == 1) {
        rep(1, 21)        # Permanent step change
      } else {
        delta^weeks       # Exponential decay
      }
      
      # Create plot data without names
      plot_data <- data.frame(
        Week = weeks,
        Effect = unname(effect)  # Ensure no named vectors
      )
      
      # Create the plot
      ggplot(plot_data, aes(x = Week, y = Effect)) +
        geom_line(color = "#6B8E23", linewidth = 1.5) +  # Olive color
        geom_point(color = "#6B8E23", size = 2.5) +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "#999999") +
        annotate("text", x = max(weeks), y = 0.5, 
                 label = "Half-effect", hjust = 1.1, vjust = -0.5, color = "#666666") +
        scale_y_continuous(limits = c(0, 1.05), 
                           breaks = seq(0, 1, by = 0.2)) +
        scale_x_continuous(breaks = seq(0, 20, by = 4)) +
        labs(
          title = paste0("Intervention Effect Over Time (δ = ", round(delta, 2),")"),
          x = "Weeks After Intervention",
          y = "Relative Effect Size"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 12, face = "bold", color = "#6B8E23"),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA)
        )
    })
  })
  
  
  # Add hover tooltip functionality
  output$hover_info <- renderUI({
    req(input$plot_hover)
    hover <- input$plot_hover
    week <- round(hover$x)
    effect <- round(hover$y, 2)
    
    if (week >= 0 && week <= 20 && effect >= 0 && effect <= 1) {
      style <- paste0("position:absolute; z-index:100; background-color:rgba(245, 245, 245, 0.95); ",
                      "left:", hover$coords_css$x + 10, "px; top:", hover$coords_css$y + 10, "px; ",
                      "padding: 5px; border: 1px solid #ddd; border-radius: 3px;")
      
      div(
        style = style,
        p(HTML(paste0("<b>Week:</b> ", week, "<br/>",
                      "<b>Effect:</b> ", effect)))
      )
    }
  })
  
}

shinyApp(ui = ui, server = server)