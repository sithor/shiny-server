# Load required libraries
library(shiny)
library(readxl)
library(forecast)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(changepoint)
library(tscount)
library(mathjaxr)
library(markdown)
library(later)
library(future)
library(promises)
library(shinyjs)
# install.packages("waiter")
library(waiter)


# Preload the Excel file
data_path   <- "./data/stmf.xlsx"
# pull off all but the first sheet
all_sheets  <- excel_sheets(data_path)[-1]

# if you just want the country lookup (one sheet)
Country_code <- read_excel("./data/Country.xlsx", sheet = 1)

# if you later want to read each sheet into a named list:
sheet_data <- lapply(all_sheets, function(sh) {
  read_excel(data_path, sheet = sh, skip = 2)
})
names(sheet_data) <- all_sheets



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
                  choices = c(
                    "None",
                    "tscount (interv_detect)",
                    "changepoint (mean and variance)",
                    "changepoint (mean only)",
                    "changepoint (variance only)"
                  )) |>
        tagAppendAttributes(class = "navy-text"),
      
      ## only show penalty choices if a changepoint method is picked
      conditionalPanel(
        condition = "input.cp_method.includes('changepoint')",
        selectInput("cp_penalty",
                    span("Penalty for changepoint detection", style = "color:navy"),
                    choices = c("BIC", "AIC", "MBIC", "Manual"),
                    selected = "BIC") |>
          tagAppendAttributes(class = "navy-text")
      ),
      conditionalPanel(
        condition = "input.cp_method.includes('tscount')",
      actionButton("optimize_delta", "Optimize Intervention",
                   style = "color: white; background-color: olive; border: none; padding: 8px 16px;"),
      h5("Show how optimal intervention is selected by maximising model fit (slow!)"),
      checkboxInput("show_aic_plot", "Show AIC vs Delta plot (slower)", value = FALSE),
      sliderInput("delta", span("Intervention to detect\n(0 = spike,\n0.8 increase and decay,\n1 = step increase)",
                                style = "color:olive"),
                  min = 0, max = 1, step = 0.01, value = 0.8) |>
        tagAppendAttributes(class = "olive-text"),
      uiOutput("half_life_info") |>
        tagAppendAttributes(style = "margin-top: 10px; font-weight: bold; color: olive;"),
      sliderInput("num_cps", span("Number of candidate change points to display:", style="color:red"),
                  min = 1, max = 20, value = 4, step = 1) |>
        tagAppendAttributes(class = "red-text")
    )),
    mainPanel(
      plotOutput("time_series_plot"),
      conditionalPanel(
      condition = "input.cp_method.includes('tscount')",
        h3("Intervention effect over time"),
        htmlOutput("half_life_info"),
        p("The intervention effect is the relative change in the expected count of deaths after the intervention."),
        p("The half-life is the time it takes for the effect to decay to half its initial value."),
        p("The plot below shows the expected effect of the intervention over time."),
       
        div(id = "interventionPlotContainer",
      plotOutput("interventionPlot", height = "200px",
                 hover = hoverOpts(id = "plot_hover", delay = 100)))), 
        
      conditionalPanel(
        condition = "input.cp_method !== 'None'",
      h3("Diagnostic plots and change point detection output"),
        htmlOutput("cp_results"),
      conditionalPanel(
        condition = "input.cp_method.includes('tscount')",  
        h4("Magnitude of intervention change test statistic"),
        plotOutput("int_plot")),
        h4("Count model: observed values vs. model predicted"),
        plotOutput("obs_vs_pred", height = "800px"),
      conditionalPanel(
        condition = "input.cp_method.includes('tscount')", 
        h4("Count model: Probability integral transform plot"),
        p("Should be uniform distribution if good model fit."),
        plotOutput("fit_plot_pit"),
        h4("Count model: Other diagnostic plots"),
        plotOutput("fit_plot")),
      
      #conditionalPanel(
       # condition = "input.cp_method.includes('changepoint')",
        # h3("Diagnostic plots and change point detection output"),
        # htmlOutput("cp_results"),
        # h4("Magnitude of intervention change test statistic"),
        # plotOutput("int_plot"),
      
      ## AIC plot (new)
      conditionalPanel(
        condition = "input.show_aic_plot == true",
        h4("Model AIC vs. Delta"),
        plotOutput("aic_plot", height = "400px")))
      #)
    )))

# Define the server logic (unchanged from your code)
server <- function(input, output, session) {
  source("function.R")
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
    
    # look up the sheet “code” based on the selected country
    sheet_code <- Country_code$Code[ Country_code$Country == input$sheet ]
    
    # read exactly that sheet, skipping the first two rows
    df <- read_excel(
      path  = data_path,
      sheet = sheet_code,
      skip  = 2,
      col_names = TRUE
    )
    
    # rename the imported columns just as before
    names(df)[5:10]  <- paste0("count_", gsub("\\..*", "", names(df)[5:10]))
    names(df)[11:16] <- paste0("rate_",  gsub("\\..*", "", names(df)[11:16]))
    
    # keep only 'both sexes' rows
    df <- filter(df, Sex == "b")
    
    df
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
    req(input$delta)
    req(data(), input$age_category, input$data_type, input$cp_method)
    if( grepl("changepoint", input$cp_method)){ 
      req(input$cp_penalty)
    }
    
    column_name <- paste0(input$data_type, "_", input$age_category)
    
    if (!column_name %in% names(data())) {
      showNotification("Invalid selection. Column not found.", type = "error")
      return(NULL)
    }
    
    start_date <- data()$Year[1] + (data()$Week[1] - 1) / 52
    time_series <- ts(data()[[column_name]], frequency = 52, start = start_date)
    
    if (input$cp_method == "tscount (interv_detect)") {
      
      ## put in waiter spinner with message 
      w <- Waiter$new(
        html = tagList(
          spin_fading_circles(),  # built-in spinner from waiter
          h3("Detecting change points using"), 
          code("tscount::interv_detect()"),
          p("This may take a minute or two...")
        ),
        color = "rgba(0,0,0,0.5)"  # semi-transparent dark overlay
      )
      w$show()
      ## Your existing tscount code
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
      w$hide()
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
      
      # inside detect_changepoints(), in the changepoint branch:
    } else if (grepl("^changepoint", input$cp_method)) {
      
      # 1. build the ts
      y      <- as.numeric(time_series)
      t      <- time(time_series)
      
      # 2. fit & remove global linear trend
      trend_mod     <- lm(y ~ t)
      trend_fit     <- as.numeric(fitted(trend_mod))
      detrended_ts  <- y - trend_fit
      
      # 3. run the changepoint detection on the residuals
      param   <- switch(input$cp_method,
                        "changepoint (mean and variance)" = "meanvar",
                        "changepoint (mean only)"          = "mean",
                        "changepoint (variance only)"      = "variance")
      cpt_func <- switch(param,
                         meanvar  = cpt.meanvar,
                         mean     = cpt.mean,
                         variance = cpt.var)
      
      cpt_result <- cpt_func(detrended_ts,
                             penalty = input$cp_penalty,
                             method  = "AMOC",
                             class   = TRUE)
      
      cps_idx     <- cpts(cpt_result)
      cpts_numeric<- time(time_series)[cps_idx]
      cpts_dates  <- date_decimal(cpts_numeric)
      
      # 4. compute segment means on the *residuals*
      bounds    <- c(0, cps_idx, length(detrended_ts))
      seg_means <- numeric(length(detrended_ts))
      for (i in seq_len(length(bounds)-1)) {
        idx <- (bounds[i]+1):bounds[i+1]
        seg_means[idx] <- mean(detrended_ts[idx], na.rm = TRUE)
      }
      
      # 5. reconstruct the full fitted values = trend + segment offsets
      fitted_vals <- trend_fit + seg_means
      
      return(list(
        method        = "changepoint",
        cpts_numeric  = cpts_numeric,
        cpts          = cpts_dates,
        object        = cpt_result,
        trend         = trend_fit,
        seg_means     = seg_means,
        fitted_values = fitted_vals
      ))
    }
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
  
  
  
  
  
  
  # (Other server outputs remain unchanged)
  output$cp_results <- renderUI({
    cp <- detect_changepoints()
    if (is.null(cp)) {
      return(HTML("<p><em>No change point detection method selected.</em></p>"))
    }
    
    lines <- list()
    
    if (cp$method == "tscount") {
      # 1) header & list of top change points
      lines[[1]] <- sprintf(
        "<p><strong>Top %d candidate change points detected by <code>tscount::interv_detect()</code>:</strong></p>",
        input$num_cps
      )
      for (i in seq_along(cp$cpts)) {
        date    <- format(cp$cpts[i], "%B %d, %Y")
        tau     <- cp$sizes[i]
        eff     <- cp$effects[i]
        dir_txt <- if (eff < 0) " decrease" else " increase"
        lines[[i+1]] <- sprintf(
          "<p>%d. %s — test statistic: %.2f, intervention effect: %.2f%%%s</p>",
          i, date, tau, abs(eff), dir_txt
        )
      }
      
      # 2) compute cumulative % increase over 0–20 weeks
      effect_0 <- cp$effects[1]      # top_indices[1] == tau_max
      delta    <- input$delta
      n_weeks  <- 20
      cum_pct  <- round( effect_0 * (1 - delta^(n_weeks+1)) / (1 - delta), 2 )
      
      # 3) get baseline from null model at the detected week
      tau_idx      <- cp$object$tau_max
      baseline_val <- cp$null_fit$fitted.values[tau_idx]
      
      # 4) estimate extra deaths
      extra_deaths <- round( baseline_val * cum_pct/100 )
      
      # 5) append those two lines
      lines[[ length(lines)+1 ]] <- sprintf(
        "<p><strong>For the first change point, the cumulative effect over 0–%d weeks
        \n (assuming all deaths occurred in one week):</strong> %s%%</p>",
        n_weeks, cum_pct
      )
      lines[[ length(lines)+1 ]] <- sprintf(
        "<p><strong>Estimated extra deaths at week %s (baseline = %.0f):</strong> %d</p>",
        format(cp$cpts[1], "%B %d, %Y"), baseline_val, extra_deaths
      )
    # changepoint package branch
    } else if (cp$method == "changepoint") {
      lines[[1]] <- "<p><strong>Detected change points (changepoint package):</strong></p>"
      # rebuild the raw series to compute before/after means
      col_name <- paste0(input$data_type, "_", input$age_category)
      ts       <- ts(data()[[col_name]],
                     frequency = 52,
                     start     = data()$Year[1] + (data()$Week[1]-1)/52)
      vals     <- as.numeric(ts)
      cps_idx      <- cpts(cp$object)
      
      
      for (i in seq_along(cps_idx)) {
        tau <- cps_idx[i]
        if (tau <= 1 || tau >= length(vals)) {
          pct_txt <- "NA"
        } else {
          before  <- mean(vals[1:tau], na.rm = TRUE)
          after   <- mean(vals[(tau+1):length(vals)], na.rm = TRUE)
          pct     <- ((after - before)/before) * 100
          pct_txt <- sprintf("%.2f%% %s",
                             abs(pct),
                             if (pct < 0) "decrease" else "increase")
        }
        date_str <- format(date_decimal(time(ts)[tau]), "%B %d, %Y")
        lines[[i+1]] <- sprintf(
          "<p>%d. %s — %s</p>",
          i, date_str, pct_txt
        )
      }
    }
    
    HTML(paste0(lines, collapse=""))
    
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
    
    # -------- NEW / FIXED
    if (!is.null(cp_result) && cp_result$method %in% c("tscount", "changepoint", "bcp")) {
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
    req(input$cp_method == "tscount (interv_detect)")
    
    # 1. pull in your CP result and original ts
    cp   <- detect_changepoints()
    col  <- paste0(input$data_type, "_", input$age_category)
    ts   <- ts(
      data()[[col]],
      frequency = 52,
      start     = data()$Year[1] + (data()$Week[1] - 1)/52
    )
    
    # 2. extract stats and align the time vector
    stats <- as.vector(cp$object$test_statistic_tau)
    times <- time(ts)[ seq_along(stats) ]  # drop that extra element
    
    # 3. build a well-formed data.frame
    df_plot <- data.frame(
      Time     = times,
      TestStat = stats
    )
    
    # 4. plot
    ggplot(df_plot, aes(x = Time, y = TestStat)) +
      geom_line() +
      labs(
        title = "Intervention Test Statistics",
        x     = "Time",
        y     = "Test statistic (τ)"
      ) +
      theme_minimal() +
      geom_vline(
        xintercept = cp$cpts_numeric,
        linetype   = "dashed",
        color      = "red"
      )
  })
  
  output$fit_plot <- renderPlot({
    req(data(), input$age_category, input$data_type, 
        input$cp_method == "tscount (interv_detect)")
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
    req(data(), input$age_category, input$data_type,
        input$cp_method == "tscount (interv_detect)")
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
    cp <- detect_changepoints()
    req(cp)
    
    # rebuild the original time series
    col_name  <- paste0(input$data_type, "_", input$age_category)
    ts_series <- ts(
      data()[[col_name]],
      frequency = 52,
      start     = data()$Year[1] + (data()$Week[1] - 1)/52
    )
    time_pts  <- time(ts_series)
    obs_vals  <- as.numeric(ts_series)
    
    # pick up the right fitted values
    fitted_vals <- switch(
      cp$method,
      tscount     = as.numeric(cp$fit$fitted.values),
      changepoint = cp$fitted_values
    )
    
    # build a single data frame
    df <- data.frame(
      Time     = time_pts,
      Observed = obs_vals,
      Fitted   = fitted_vals
    )
    
    # plot them
    ggplot(df, aes(x = Time)) +
      geom_line(aes(y = Observed, color = "Observed")) +
      geom_line(aes(y = Fitted,   color = "Fitted"), linetype = "dashed") +
      scale_color_manual(NULL,
                         values = c(Observed = "black", Fitted = "blue")
      ) +
      labs(
        title = paste0(
          if (cp$method=="tscount") "tscount model: " else "changepoint+trend model: ",
          "Observed vs Fitted"
        ),
        x     = "Year",
        y     = "Weekly count (deaths)"
      ) +
      theme_minimal(base_size = 14)
  })
  
  ## Optimise delta code----
  # Function to find optimal delta
  
  # --- Optimization function without its own withProgress wrapper ---
  # --- Parallel optimization function ---
  find_optimal_delta_optimize <- function() {
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
    
    # Objective function: AIC as a function of delta
    objective <- function(delta) {
      if (delta < 0 || delta > 1) return(Inf)  # outside bounds
      tryCatch({
        fit <- suppressWarnings(tsglm(ts = time_series,
                                      xreg = regressors,
                                      model = list(past_obs = c(1, 52)),
                                      distr = "nbinom", link = "log",
                                      info = "score"))
        if (is.null(fit)) return(Inf)
        
        interv_result <- suppressWarnings(interv_detect(fit, delta = delta, est_interv = TRUE))
        if (is.null(interv_result)) return(Inf)
        
        if (interv_result$tau_max == 1 || interv_result$tau_max == length(time_series)) {
          return(Inf)  # tau at boundary, bad model
        }
        
        intervention_indicator <- interv_covariate(n = length(time_series), tau = interv_result$tau_max, delta = delta)
        fit_i <- suppressWarnings(tsglm(ts = time_series,
                                        xreg = cbind(linearTrend = seq(along = time_series) / 52,
                                                     intervention = intervention_indicator),
                                        model = list(past_obs = c(1, 52)),
                                        distr = "nbinom", link = "log",
                                        info = "score"))
        AIC(fit_i)
      }, error = function(e) Inf)
    }
    
    # Now use optimize to find delta that minimizes AIC
    res_opt <- optimize(f = objective, lower = 0, upper = 1)
    
    best_delta <- res_opt$minimum
    best_aic <- res_opt$objective
    
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
        h3("Optimizing delta function"),
        p("using Nelder-Mead algorithm (takes a minute or two)...")
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
        find_optimal_delta_optimize()
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
    req(input$delta, input$cp_method == "tscount (interv_detect)")
    
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
  output$aic_plot <- renderPlot({
    req(input$show_aic_plot)  # only run if checkbox ticked
    req(data(), input$age_category, input$data_type, 
        input$cp_method == "tscount (interv_detect)")
    
    aic_data <- evaluate_aic_over_grid(
      current_data = data(),
      current_age = input$age_category,
      current_cp_method = input$cp_method,
      current_data_type = input$data_type,
      n_points = 30
    )
    
    if (is.null(aic_data)) return(NULL)
    
    ggplot(aic_data, aes(x = delta, y = AIC)) +
      geom_line(color = "blue", linewidth = 1.2) +
      geom_point(color = "blue") +
      geom_vline(xintercept = aic_data$delta[which.min(aic_data$AIC)], color = "red", linetype = "dashed") +
      annotate("text", x = aic_data$delta[which.min(aic_data$AIC)], 
               y = min(aic_data$AIC, na.rm = TRUE),
               label = paste0("Optimal δ ≈ ", round(aic_data$delta[which.min(aic_data$AIC)], 2)),
               vjust = -1, color = "red", size = 4) +
      labs(
        title = "Model AIC vs. Delta",
        x = "Delta (Decay Parameter)",
        y = "AIC (Model Fit)"
      ) +
      theme_minimal(base_size = 14)
  })
  
  
  # Output for AIC vs delta plot
  output$aic_plot <- renderPlot({
    req(input$show_aic_plot)
    req(data(), input$age_category, input$data_type, input$cp_method == "tscount (interv_detect)")
    
    # Attach a waiter spinner
    w <- Waiter$new(
      html = tagList(
        spin_fading_circles(),  # built-in spinner from waiter
        h3("Calculating AIC [model fit] vs. Delta"),
        p("Illustrates delta selection...")
      ),
      color = "rgba(0,0,0,0.5)"  # semi-transparent dark overlay
    )
    w$show()
    
    # Actual computation
    aic_data <- evaluate_aic_over_grid(
      current_data = data(),
      current_age = input$age_category,
      current_cp_method = input$cp_method,
      current_data_type = input$data_type,
      n_points = 30
    )
    
    w$hide()
    
    if (is.null(aic_data)) return(NULL)
    
    ggplot(aic_data, aes(x = delta, y = AIC)) +
      geom_line(color = "blue", linewidth = 1.2) +
      geom_point(color = "blue") +
      geom_vline(xintercept = aic_data$delta[which.min(aic_data$AIC)], color = "red", linetype = "dashed") +
      annotate("text", x = aic_data$delta[which.min(aic_data$AIC)], 
               y = min(aic_data$AIC, na.rm = TRUE),
               label = paste0("Optimal δ ≈ ", round(aic_data$delta[which.min(aic_data$AIC)], 2)),
               vjust = -1, color = "red", size = 4) +
      labs(
        title = "Model AIC vs. Delta",
        x = "Delta (Decay Parameter)",
        y = "AIC (Model Fit)"
      ) +
      theme_minimal(base_size = 14)
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