library(shiny)
library(ggplot2)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Lato"),
    heading_font = font_google("Montserrat"),
    primary = "#2C3E50",
    secondary = "#18BC9C"
  ),
  titlePanel("Statistical Power Calculator (power.prop.test)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("solve_for", "Solve for:", choices = c("power", "n", "p1", "p2"), selected = "power"),
      conditionalPanel(condition = "input.solve_for != 'n'",
                       sliderInput("n", "Sample size per group (n):", min = 10, max = 1000, value = 100, step = 10)),
      conditionalPanel(condition = "input.solve_for != 'p1'",
                       sliderInput("p1", "Proportion in group 1 (p1):", min = 0.01, max = 0.99, value = 0.5, step = 0.01)),
      conditionalPanel(condition = "input.solve_for != 'p2'",
                       sliderInput("p2", "Proportion in group 2 (p2):", min = 0.01, max = 0.99, value = 0.6, step = 0.01)),
      conditionalPanel(condition = "input.solve_for != 'power'",
                       sliderInput("power", "Target power:", min = 0.01, max = 0.99, value = 0.8, step = 0.01)),
      sliderInput("alpha", "Significance level (alpha):", min = 0.001, max = 0.2, value = 0.05, step = 0.001),
      uiOutput("xvarUI")
    ),
    mainPanel(
      h4("Calculated Result"),
      verbatimTextOutput("resultText"),
      plotOutput("resultPlot")
    )
  )
)

server <- function(input, output, session) {
  output$xvarUI <- renderUI({
    req(input$solve_for)
    labels <- c(
      n = "Sample size per group (n)",
      p1 = "Proportion in group 1 (p1)",
      p2 = "Proportion in group 2 (p2)",
      power = "Target power",
      alpha = "Significance level (alpha)"
    )
    valid <- names(labels) != input$solve_for
    selectInput("xvar", "X-axis variable:", choices = setNames(names(labels)[valid], labels[valid]), selected = names(labels)[valid][1])
  })
  
  output$resultText <- renderPrint({
    req(input$solve_for, input$alpha)
    args <- list(alternative = "two.sided", sig.level = input$alpha)
    if (input$solve_for != "n")     args$n     <- input$n
    if (input$solve_for != "p1")    args$p1    <- input$p1
    if (input$solve_for != "p2")    args$p2    <- input$p2
    if (input$solve_for != "power") args$power <- input$power
    args[[input$solve_for]] <- NULL
    res <- tryCatch(do.call(power.prop.test, args), error = function(e) e)
    if (inherits(res, "error")) paste("Error:", res$message) else round(res[[input$solve_for]], 4)
  })
  
  output$resultPlot <- renderPlot({
    req(input$solve_for, input$xvar, input$alpha)
    # Labels lookup
    axis_labels <- c(
      n = "Sample size per group (n)",
      p1 = "Proportion in group 1 (p1)",
      p2 = "Proportion in group 2 (p2)",
      power = "Target power",
      alpha = "Significance level (alpha)"
    )
    # Sequence for x-axis
    xvals <- switch(input$xvar,
                    n     = seq(10, 1000, by = 10),
                    p1    = seq(0.01, 0.99, by = 0.01),
                    p2    = seq(0.01, 0.99, by = 0.01),
                    power = seq(0.01, 0.99, by = 0.01),
                    alpha = seq(0.001, 0.2, by = 0.001))
    # Compute y-values
    yvals <- sapply(xvals, function(xv) {
      args <- list(alternative = "two.sided", sig.level = input$alpha)
      if (input$solve_for != "n")     args$n     <- input$n
      if (input$solve_for != "p1")    args$p1    <- input$p1
      if (input$solve_for != "p2")    args$p2    <- input$p2
      if (input$solve_for != "power") args$power <- input$power
      if (input$xvar == "alpha") {
        args$sig.level <- xv
      } else {
        args[[input$xvar]] <- xv
      }
      args[[input$solve_for]] <- NULL
      res <- tryCatch(do.call(power.prop.test, args), error = function(e) NULL)
      if (is.null(res)) NA else res[[input$solve_for]]
    })
    df <- data.frame(x = xvals, y = yvals)
    df <- df[!is.na(df$y), ]
    if (nrow(df) == 0) {
      ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid data for these parameters", size = 6, hjust = 0.5) + theme_void()
    } else {
      y_range <- range(df$y)
      pad <- diff(y_range) * 0.05
      y_limits <- c(y_range[1] - pad, y_range[2] + pad)
      current_x <- switch(input$xvar, n = input$n, p1 = input$p1, p2 = input$p2, power = input$power, alpha = input$alpha)
      args_cur <- list(alternative = "two.sided", sig.level = input$alpha)
      if (input$solve_for != "n")     args_cur$n     <- input$n
      if (input$solve_for != "p1")    args_cur$p1    <- input$p1
      if (input$solve_for != "p2")    args_cur$p2    <- input$p2
      if (input$solve_for != "power") args_cur$power <- input$power
      if (input$xvar == "alpha") args_cur$sig.level <- current_x else args_cur[[input$xvar]] <- current_x
      args_cur[[input$solve_for]] <- NULL
      res_cur <- tryCatch(do.call(power.prop.test, args_cur), error = function(e) NULL)
      current_y <- if (!is.null(res_cur)) res_cur[[input$solve_for]] else NA
      p <- ggplot(df, aes(x = x, y = y)) +
        geom_line(linewidth = 1.2) +
        labs(
          x = axis_labels[input$xvar],
          y = paste("Estimated", input$solve_for),
          title = paste("Variation of", input$solve_for, "vs", axis_labels[input$xvar])
        ) +
        theme_bw(base_size = 16) +
        theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14)) +
        scale_y_continuous(limits = y_limits)
      if (!is.na(current_x) && !is.na(current_y)) {
        p <- p + geom_point(data = data.frame(x = current_x, y = current_y),
                            aes(x = x, y = y), shape = 4, size = 4, stroke = 2,
                            colour = "red")
        }
      p
    }
  })
}

shinyApp(ui, server)




