#
# ──────────────────────────────────────────────────────────────────────
# 0) Load libraries
library(shiny)
library(bnlearn)
library(gRain)
library(graph)
library(dagitty)
library(ggplot2)
library(igraph)
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
if (!requireNamespace("Rgraphviz", quietly = TRUE)) BiocManager::install("Rgraphviz")

# ──────────────────────────────────────────────────────────────────────
# 1) Load your saved objects from dag.rdata
#    This must happen in the global scope, so that Shiny’s server() can see 'dag' and 'CD_bn'.
load("dag.rdata")   # now we have 'dag' (class "bn") and 'CD_bn' (data.frame)

# Check that 'dag' really is a bn object:
if (!inherits(dag, "bn")) {
  stop("Loaded object 'dag' is not of class 'bn'.")
}

# ──────────────────────────────────────────────────────────────────────
# 2) Clean and coerce CD_bn to factors in the same order as nodes(dag)
CD_bn <- na.omit(CD_bn)
if (nrow(CD_bn) == 0) {
  stop("No complete cases in CD_bn (global check).")
}

# Ensure that CD_bn has exactly the same columns as nodes(dag):
bn_nodes <- bnlearn::nodes(dag)
if (!all(bn_nodes %in% colnames(CD_bn))) {
  stop(
    "DAG nodes do not match CD_bn columns (global check): ",
    paste(setdiff(bn_nodes, colnames(CD_bn)), collapse = ", ")
  )
}

# Subset and coerce each column to factor, in the order of bn_nodes:
CD_bn <- as.data.frame(
  lapply(CD_bn[, bn_nodes], as.factor),
  stringsAsFactors = FALSE
)

# Build node_levels for the UI (each node’s factor levels):
node_levels <- lapply(CD_bn, levels)

# ──────────────────────────────────────────────────────────────────────
# 3) Build a single string "dag { A -> B\nC -> D\n… }" for dagitty()
#    Do NOT call dagitty() on individual lines—assemble one multi-line string first.
edge_lines <- apply(bnlearn::arcs(dag), 1, function(x) {
  paste0(x[1], " -> ", x[2])
})
dag_string <- paste0(
  "dag {\n",
  paste(edge_lines, collapse = "\n"),
  "\n}"
)

# Now create the dagitty object once, globally:
dagitty_dag <- tryCatch({
  dagitty(dag_string)
}, error = function(e) {
  stop("Error building dagitty_dag from string:\n", dag_string, "\nError was: ", e$message)
})

# Verify that dagitty_dag is valid:
if (!inherits(dagitty_dag, "dagitty")) {
  stop("dagitty_dag is not a dagitty object.")
}

# ──────────────────────────────────────────────────────────────────────
# 4) Everything above this line is global. Below this line, start your UI
#    (which can reference node_levels, dag, CD_bn, and dagitty_dag directly).
#
# ui <- fluidPage( … )
# server <- function(input, output, session) { … }
# shinyApp(ui, server)
# ──────────────────────────────────────────────────────────────────────

# ──────────────────────────────────────────────────────────────────────
# 4) UI definition can now safely use node_levels:
ui <- fluidPage(
  titlePanel("Bayesian Network: DAG + Posterior Updates"),
  sidebarLayout(
    sidebarPanel(
      h4("Set Evidence"),
      uiOutput("evidence_selectors"),
      hr(),
      h4("Causal Inference"),
      div(
        style = "background: #e0ccff; padding: 4px 8px; margin-bottom: 8px; border-radius: 4px;",
        selectInput(
          "exposure", 
          "Exposure", 
          choices = bnlearn::nodes(dag),
          width = "100%"
        )
      ),
      div(
        style = "background: #f8d3d3; padding: 4px 8px; margin-bottom: 8px; border-radius: 4px;",
        selectInput(
          "outcome", 
          "Outcome", 
          choices = bnlearn::nodes(dag),
          selected = bnlearn::nodes(dag)[2],
          width = "100%"
        )
      ),
      checkboxInput("all_colliders", "Show all colliders", value = FALSE),
      uiOutput("backdoor_selector"),
      downloadButton("downloadPlot", "Download DAG Plot (PNG)"),
      hr(),
      h4("Legend"),
      uiOutput("colorLegend")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Posterior distributions",
          h4("Posterior distributions given evidence"),
          plotOutput("posteriorBars", height = "800px", width = "100%"),
          verbatimTextOutput("colliderText"),
          verbatimTextOutput("pathInfo"),
          uiOutput("posteriorLabels")
        ),
        tabPanel(
          "DAG structure",
          h4("DAG structure"),
          plotOutput("dagStructure", height = "800px", width = "100%")
        )
      )
    )
  )
)
# ──────────────────────────────────────────────────────────────────────

# ──────────────────────────────────────────────────────────────────────
# 5) Server logic (no need to redefine node_levels here)
server <- function(input, output, session) {
  # Reactive to collect evidence
  getEvidence <- reactive({
    ev <- list()
    for (node in names(node_levels)) {
      val <- input[[paste0("ev_", node)]]
      if (!is.null(val) && val != "None") {
        ev[[node]] <- val
      }
    }
    ev
  })
  
  # Fit the BN + compile the gRain network once
  fitted         <- bn.fit(dag, data = CD_bn, method = "bayes")
  grain_obj      <- as.grain(fitted)
  compiled_grain <- compile(grain_obj)
    
  output$posteriorLabels <- renderUI({
    req(grain_obj)
    
    marginals <- tryCatch({
      querygrain(grain_obj, type = "marginal")
    }, error = function(e) {
      showNotification(paste("Error querying marginals:", e$message), type = "error")
      NULL
    })
    if (is.null(marginals)) return(NULL)
    
    tagList(
      tags$div(
        style = "background:#f0f0f0;
               padding:10px;
               margin-bottom:10px;
               border:1px solid #ccc;
               font-family: monospace; 
               font-size: 13px;",
        tags$strong("Posterior summaries:"),
        lapply(names(marginals), function(node) {
          probs <- marginals[[node]]
          top   <- names(probs)[which.max(probs)]
          val   <- round(max(probs), 3)
          tags$div(paste0(node, ": ", top, " (", val, ")"))
        })
      )
    )
  })
  
  getEvidence <- reactive({
    ev <- list()
    for (node in names(node_levels)) {
      val <- input[[paste0("ev_", node)]]
      if (!is.null(val) && val != "None") {
        ev[[node]] <- val
      }
    }
    ev
  })
  
  grain_with_evidence <- reactive({
    ev <- getEvidence()
    if (length(ev) == 0) return(compiled_grain)
    tryCatch({
      setEvidence(compiled_grain, nodes = names(ev), states = unname(ev))
    }, error = function(e) {
      showNotification(paste("Error setting evidence:", e$message), type = "error")
      compiled_grain
    })
  })
  
  backdoor_sets <- reactive({
    exp <- input$exposure
    out <- input$outcome
    if (exp == out || is.null(exp) || is.null(out)) return(list())
    tryCatch({
      sets <- adjustmentSets(dagitty_dag, exposure = exp, outcome = out)
      if (!is.list(sets)) sets <- list(sets)
      sets
    }, error = function(e) {
      message("Error in adjustmentSets: ", e$message)
      list()
    })
  })
  
  output$backdoor_selector <- renderUI({
    sets <- backdoor_sets()
    if (length(sets) == 0) return(helpText("No adjustment needed."))
    selectInput("adjust_set_index", "Select Adjustment Set", choices = seq_along(sets), selected = 1)
  })
  
  current_adjustment_set <- reactive({
    sets <- backdoor_sets()
    idx <- input$adjust_set_index
    if (!is.null(sets) && !is.null(idx) && idx <= length(sets)) {
      adj_set <- sets[[as.numeric(idx)]]
      if (is.list(adj_set)) adj_set <- unlist(adj_set, recursive = TRUE)
      adj_set
    } else {
      character(0)
    }
  })
  
  colliders_between <- reactive({
    exp <- input$exposure
    out <- input$outcome
    if (exp == out || is.null(exp) || is.null(out)) return(character(0))
    tryCatch({
      paths <- dagitty::paths(dagitty_dag, from = exp, to = out, directed = FALSE)$paths
      colliders <- c()
      for (p in paths) {
        tokens <- strsplit(p, " ")[[1]]
        for (i in seq(2, length(tokens) - 2, by = 2)) {
          left <- tokens[i]
          mid <- tokens[i + 1]
          right <- tokens[i + 2]
          if (left == "->" && right == "<-") colliders <- c(colliders, mid)
        }
      }
      unique(colliders)
    }, error = function(e) {
      message("Error in colliders_between: ", e$message)
      character(0)
    })
  })
  
  output$evidence_selectors <- renderUI({
    req(node_levels)
    exp_node <- input$exposure
    out_node <- input$outcome
    
    adj_nodes <- current_adjustment_set()
    colliders <- colliders_between()
    
    get_node_color <- function(node) {
      if (!is.null(node) && node == exp_node) return("#e0ccff")   # exposure
      if (!is.null(node) && node == out_node)  return("#f8d3d3")   # outcome
      if (node %in% adj_nodes)                   return("#d0e7ff") # adjustment
      if (node %in% colliders)                   return("#ffe6b3") # collider
      return("white")
    }
    
    selectors <- lapply(names(node_levels), function(node) {
      bg <- get_node_color(node)
      div(
        style = sprintf("background:%s; padding:4px 8px; margin-bottom:4px; border-radius:4px;", bg),
        selectInput(
          inputId = paste0("ev_", node),
          label   = node,
          choices = c("None", node_levels[[node]]),
          selected = "None",
          width = "100%"
        )
      )
    })
    
    do.call(tagList, selectors)
  })
  
  
  # In the server, inside renderPlot (only showing the relevant bits):
  output$dagStructure <- renderPlot({
    # 1) Build node_fill exactly as before
    all_nodes <- bnlearn::nodes(dag)
    node_fill <- rep("white", length(all_nodes))
    names(node_fill) <- all_nodes
    
    if (!is.null(input$exposure) && input$exposure %in% all_nodes) {
      node_fill[input$exposure] <- "#e0ccff"
    }
    if (!is.null(input$outcome) && input$outcome %in% all_nodes) {
      node_fill[input$outcome] <- "#f8d3d3"
    }
    adj_set <- current_adjustment_set()
    if (!is.null(adj_set) && length(adj_set) > 0 && all(adj_set %in% all_nodes)) {
      node_fill[adj_set] <- "#d0e7ff"
    }
    colliders <- colliders_between()
    if (!is.null(colliders) && length(colliders) > 0 && all(colliders %in% all_nodes)) {
      node_fill[colliders] <- "#ffe6b3"
    }
    
    tryCatch({
      # 2) Convert to igraph and explicitly assign vertex colours
      ig <- bnlearn::as.igraph(dag)
      V(ig)$color <- node_fill[V(ig)$name]
      
      # 3) Plot using igraph’s own layout, without re-passing vertex.color
      par(mar = c(1,1,1,1))
      plot(
        ig,
        vertex.label        = V(ig)$name,
        vertex.label.family = "sans",
        vertex.label.cex    = 0.8,
        vertex.size         = 40,
        edge.arrow.size     = 0.5,
        layout              = layout_with_sugiyama(ig)$layout
        # note: vertex.color is already stored in V(ig)$color
      )
    }, error = function(e) {
      message("Plotting error: ", e$message)
      plot.new()
      text(0.5, 0.5, paste("Error rendering DAG:", e$message), col = "red")
    })
  }, res = 96)
  
  output$posteriorBars <- renderPlot({
    # ─────────────────────────────────────────────────────────────
    # 1) Rebuild the grain object under current evidence:
    grain_obj <- grain_with_evidence()
    req(grain_obj)
    
    marginals <- tryCatch(
      querygrain(grain_obj, nodes = names(fitted), type = "marginal"),
      error = function(e) {
        showNotification("Error querying marginals: ", e$message, type = "error")
        return(NULL)
      }
    )
    req(!is.null(marginals))
    
    # 2) Grab the raw evidence list (e.g. list(Sex="Female", Smoke="Yes", …))
    ev <- getEvidence()
    
    # 3) Build a data.frame with exactly one row per (node, state)
    plot_data <- do.call(rbind, lapply(names(node_levels), function(node) {
      levels_i <- node_levels[[node]]  # all possible states for this node
      
      if (node %in% names(ev)) {
        # If the user has set evidence on this node, force 1.0 at that state, 0 for others
        forced_state <- ev[[node]]
        probs <- setNames(numeric(length(levels_i)), levels_i)
        if (forced_state %in% levels_i) {
          probs[forced_state] <- 1
        }
      } else {
        # Otherwise, use the marginal from querygrain, padded with zeros if needed
        dist_i <- marginals[[node]]
        probs <- setNames(numeric(length(levels_i)), levels_i)
        for (lvl in names(dist_i)) {
          if (lvl %in% levels_i) {
            probs[lvl] <- as.numeric(dist_i[lvl])
          }
        }
      }
      
      data.frame(
        node  = node,
        state = levels_i,
        prob  = as.numeric(probs),
        stringsAsFactors = FALSE
      )
    }))
    
    # 4) Assign fill colours that match your sidebar‐input backgrounds:
    plot_data$fill_color <- "white"  # default for “unhighlighted” nodes
    
    # Exposure → same as the exposure‐input background (#e0ccff)
    plot_data$fill_color[plot_data$node == input$exposure] <- "#e0ccff"
    
    # Outcome → same as the outcome‐input background (#f8d3d3)
    plot_data$fill_color[plot_data$node == input$outcome] <- "#f8d3d3"
    
    # Adjustment set → same as the adj‐selector background (#d0e7ff)
    adj <- current_adjustment_set()
    if (!is.null(adj) && length(adj) > 0) {
      plot_data$fill_color[plot_data$node %in% adj] <- "#d0e7ff"
    }
    
    # Colliders → same as the collider‐selector background (#ffe6b3)
    cols <- colliders_between()
    if (!is.null(cols) && length(cols) > 0) {
      plot_data$fill_color[plot_data$node %in% cols] <- "#ffe6b3"
    }
    
    # 5) Draw one bar per (node, state) in a facet for each node—no stacking:
    ggplot(plot_data, aes(x = state, y = prob)) +
      geom_col(aes(fill = fill_color), color = "black", width = 0.8) +
      facet_wrap(~ node, scales = "free_x") +
      scale_fill_identity() +
      labs(
        title = "Posterior Distributions (Evidence fixed at 100%)",
        y     = "Probability",
        x     = "State"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1),
        strip.text   = element_text(size = 12, face = "bold")
      )
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() paste0("dag_structure_", Sys.Date(), ".png"),
    content = function(file) {
      png(file, width = 1600, height = 1200, res = 150)
      all_nodes <- nodes(dag)
      fill_col <- rep("white", length(all_nodes))
      names(fill_col) <- all_nodes
      if (!is.null(input$exposure) && input$exposure %in% all_nodes) {
        fill_col[input$exposure] <- "#e0ccff"
      }
      if (!is.null(input$outcome) && input$outcome %in% all_nodes) {
        fill_col[input$outcome] <- "#f8d3d3"
      }
      adj_set <- current_adjustment_set()
      if (!is.null(adj_set) && length(adj_set) > 0 && all(adj_set %in% all_nodes)) {
        fill_col[adj_set] <- "#d0e7ff"
      }
      colliders <- colliders_between()
      if (!is.null(colliders) && length(colliders) > 0 && all(colliders %in% all_nodes)) {
        fill_col[colliders] <- "#ffe6b3"
      }
      tryCatch({
        plot(dag,
             vertex.label = all_nodes,
             vertex.color = fill_col,
             vertex.label.family = "sans",
             vertex.label.cex = 1,
             vertex.size = 30,
             edge.arrow.size = 0.5,
             layout = layout_with_sugiyama(dag)$layout)
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error rendering DAG:", e$message), col = "red")
      })
      dev.off()
    }
  )
  
  output$colorLegend <- renderUI({
    exp <- input$exposure
    out <- input$outcome
    adj <- current_adjustment_set()
    colliders <- tryCatch(colliders_between(), error = function(e) character(0))
    tags$div(style = "font-size: 14px;",
             # Exposure = #e0ccff
             tags$p(
               tags$span(style = "color: #e0ccff; font-weight: bold;", "■"),
               " Exposure"
             ),
             # Outcome = #f8d3d3
             tags$p(
               tags$span(style = "color: #f8d3d3; font-weight: bold;", "■"),
               " Outcome"
             ),
             # Adjustment set = #d0e7ff (only if there’s at least one)
             if (!is.null(adj) && length(adj) > 0) tags$p(
               tags$span(style = "color: #d0e7ff; font-weight: bold;", "■"),
               paste("Adjustment set:", paste(adj, collapse = ", "))
             ),
             # Colliders = #ffe6b3 (only if there’s at least one)
             if (!is.null(colliders) && length(colliders) > 0) tags$p(
               tags$span(style = "color: #ffe6b3; font-weight: bold;", "■"),
               paste("Colliders:", paste(colliders, collapse = ", "))
             )
    )
  })
  
  output$colliderText <- renderText({
    colliders <- tryCatch(colliders_between(), error = function(e) character(0))
    exp <- input$exposure
    out <- input$outcome
    if (length(colliders) == 0) {
      return(paste("No colliders found on paths between", exp, "and", out))
    } else {
      return(paste("Colliders between", exp, "and", out, ":\n",
                   paste(colliders, collapse = ", "),
                   "\n\nWarning: Conditioning on these may introduce collider bias."))
    }
  })
  
  output$pathInfo <- renderText({
    exp <- input$exposure
    out <- input$outcome
    if (exp == out || is.null(exp) || is.null(out)) return("")
    tryCatch({
      paths <- dagitty::paths(dagitty_dag, from = exp, to = out, directed = FALSE)
      paste("All paths between", exp, "and", out, ":\n",
            paste(paths$paths, collapse = "\n"),
            "\n\nCollider logic checks for -> node <- structure.")
    }, error = function(e) {
      paste("Error analyzing paths:", e$message)
    })
  })
}

shinyApp(ui, server)
