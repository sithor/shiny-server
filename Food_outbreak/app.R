### latest version
library(pacman)
library(shiny); library(flextable); library(gmodels); library(gdata); library(visdat);
library(mvbutils); library(tableone); library(xtable); library(openxlsx)
library(visreg)
library(ggplot2)
library(rvest)
library(svUnit)
library(magrittr)
library(epiDisplay)
library(lattice)
library(shinyWidgets)
library(epiR)



ui <- fluidPage(
  
  
  # App title ----
  titlePanel("Analyse your food-borne outbreak data"),
  tabsetPanel(
  tabPanel("Data input and summary.",
    
  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      
      radioButtons(
      "fileType_Input",
        label = h4("Choose File type"),
        choices = list(".csv/txt" = 1, ".xlsx" = 2),
        selected = 1,
        inline = TRUE
        ),

      # Input: Select a file ----
      fileInput("file1", "Choose file",
                multiple = TRUE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv",".xlsx")),


      # Horizontal line ----
      tags$hr(),

      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),

      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                  selected = ","),

      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      
      # Input: Strings for missing ----
      radioButtons("missing", "Missing",
                   choices = c(`Blank ("")` = "",
                               `R format ("NA")` = "NA",
                               `Full stop (".")` = "."),
                   selected = ""),
      
      # Horizontal line ----
      tags$hr(),

      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "all")),

       # Horizontal line ----

    
    # Main panel for displaying outputs ----
    mainPanel(
    br(),
    br(),
      "\n This data frame is expected
    to have consistent coding of exposures.",
    br(),
    br(),
    "For example, 
    all exposures should be consistently coded as 'yes' or 'no', 'True' or 'False'
    or other labels with two levels.",
    br(),
    br(),
      
      # Output: Data file ----
      tableOutput("contents"),
      
      tags$hr(),
      
      verbatimTextOutput("summary_df")))
    ), # tabPanel ending
  
    tabPanel("Detect duplicates", sidebarLayout(sidebarPanel(uiOutput("duplicate_vars")),
        mainPanel(
        br(),
        br(),
        "It is worthwhile checking your data for duplicates. 
        Choose a unique identifier, or combination of identifiers to check for possible duplicate entries.",
        br(),
        br(),
        "If you find duplicate entries, it is worthwhile looking in the original file.",
        br(),
        br(), 
      tags$hr(),
      
        h3(textOutput("Duplicates_or_not"),
           tags$hr(),
          tableOutput("Duplicates_print"))
     
    ))),
    
    ## next tabPanel
  tabPanel("Visualise data",
           verticalLayout(
             "Plot of spreadsheet with variables displayed along horizontal axis.\n
              Black cells indicating missing observations. \n",
              "Check for suspicious patterns that may indicate a \n
              need to recheck data in spreadsheet \n
              for data entry errors.\n",
              "The plot sorts variables (columns) with those with the most missing \n
             on the left and vice versa",
             plotOutput("missing_data_plot", width = "100%", height = "800px"),
             tags$hr(),
             "This plot shows the types of data in your input file. \n",
             
             "'Factor' means categorical variable. \n",
             
             "'Numeric' means continuous. \n",
             
             "Grey cells indicate missing observations.\n",
             
             "The plot sorts the observations (rows) with those with the most missing on the top \n
             and vice versa.",
             
             plotOutput("variable_type_plot", width = "100%", height = "800px")
           )),
  
  tabPanel("Summary of associations",
    sidebarLayout(
      sidebarPanel(
        tags$hr(),
        
        uiOutput("case_var"),
        
        tags$hr(),
        
        uiOutput("is_case"),
        
        tags$hr(),
        
        uiOutput("is_exposed"),
        
        tags$hr(),
        
        uiOutput("exposure_vars"),
        
        tags$hr()),
        
    
    mainPanel("I prefer the use of odds ratios, over relative risks in analysing food borne outbreak data.",
              "Odds ratios are not bounded by the prevalence of the disease in the unexposed cases.",
              "Relative risks, conversely, are bounded by the prevalence of disease in the unexposed.",
              tableOutput("tabpct")))),
 
   ## Panel with epidemic curve
    tabPanel("Plot histogram of onset times of cases (epicurve)",
      sidebarLayout(
        sidebarPanel(
          uiOutput("onset_time"),
          
          tags$hr(),
          
          uiOutput("date_format"),
        
          tags$hr(),
          
          uiOutput("break_hist")),
          
          mainPanel(plotOutput("epicurve", width = "100%", height = "800px"),
        
           tags$hr(),
        
          textOutput("mean_onset_date")))),
  
 
  
    tabPanel("Summary table comparing cases with non-cases",
             sidebarLayout(
               sidebarPanel(
                uiOutput("case_var_summ"),
                
                tags$hr(),
                
                uiOutput("is_case_summ"),
                
                tags$hr(),
                
                uiOutput("exposure_vars_summ")
               ),
               mainPanel(tableOutput("compare_characteristics_by_case_status"))
             ))
  
      
            )) ## 
      
      
    
      
  



# Define server logic to read selected file ----
server <- function(input, output) {
  

  ###########################################################################3
  ## functions  
import_csv <- function(file = input$file1$datapath, header = input$header, sep = input$sep, quote = input$quote,
    missing = input$missing, ...){
  if(is.null(input$file1$datapath)) return()
    read.csv(file = file, header = header, sep = sep, quote = quote, na.strings = missing , ...)
  }
  
  import_excel <- function(file = input$file1$datapath, header = input$header, 
         missing = input$missing){
    read.xlsx(xlsxFile = file, sheet = 1, na.strings = missing, detectDates = TRUE)
    
  }  
  
 import_data <- function(file = input$file1$datapath, header = input$header, sep = input$sep, quote = input$quote,
    missing = input$missing, fileType_Input = input$fileType_Input, ...){
   
   if (fileType_Input == 1) {
    df <- import_csv()
    } else {
  df <- import_excel()
  }
   
  character_vars <- sapply(df[, ], is.character) %>% as.vector
  character_vars <- names(df)[character_vars]
    
  df[character_vars] <- lapply(df[character_vars] , as.factor)
   return(df)
  }
  
  check_for_two_levels <- function(x){
    y = ifelse(nlevels(x) == 2, TRUE, FALSE)
    return(y)
  }
  
  
  
  ## three dp
  threedp <- function(x){
    formatC(x, format = "f", digits = 3 )
  }
  
  
  ## truncate numbers to two decimal places.
    twodp <- function(x){
    formatC(x, format = "f", digits = 2 )
  }
  
  onedp <- function(x){
    formatC(x, format = "f", digits = 1 )
  }
  
  format_p_value <- function(x){
    ifelse(x < 0.001, "<0.001", x %>% threedp)
  }
  
  select_binary_vars_from_dataframe <- function(df){
    
    function_or_character_var <- function(x){
      x <- ifelse(x %>% is.character|x %>% is.factor, TRUE, FALSE)
    }
    
    character_vars <- sapply(df, function_or_character_var) %>% as.vector
    character_vars <- names(df)[character_vars]
    df <- df[, character_vars]
    bin_vars <- sapply(df, check_for_two_levels) %>% as.vector
    bin_vars <- names(df)[bin_vars]
    df <- df[, bin_vars]
  }
  
  
  
  ## function takes data.frame as input and outputs summary tables of associations
  ## including ORs and PARs and confidence intervals
  summary_table_or <- function(file = input$file1$datapath, header = input$header,
                              sep = input$sep, quote = input$quote, 
                               exposure_vars = input$exposure_vars, 
                               case_var = input$case_var, is_case = input$is_case,
                              is_exposed = input$is_exposed){
    print("Is data frame imported?")
    df <- import_data()
    
    ## Convert characters to factors
    character_vars <- sapply(df[, ], is.character) %>% as.vector
    character_vars <- names(df)[character_vars]
    df[character_vars] <- lapply(df[character_vars] , as.factor)
    ###############################################################
    checkTrue(df |> is.data.frame(), "check data.frame import")  |>  print()
    print("data is imported")
    df[ , c(case_var, exposure_vars[1]) ] |>  print()
    
    ret_dfi <- as.list(1:length(exposure_vars))
    
    n_exposed_case <- 1:length(exposure_vars)
    n_unexposed_case <- 1:length(exposure_vars)
    n_exposed_non_case <- 1:length(exposure_vars) 
    n_unexposed_non_case <- 1:length(exposure_vars)
    p.value <- 1:length(exposure_vars)
    table_or <- as.list(1:length(exposure_vars))
    tab <- as.list(1:length(exposure_vars))
    PAR_alt <- as.list(1:length(exposure_vars))
    PAR <- 1:length(exposure_vars)
    
    print('table variables set-up')
    
    for (i in 1:length(exposure_vars)){
      dfi <- na.omit( df[ , c(case_var, exposure_vars[i]) ] )
      
      stopifnot(length(dfi[, c(case_var)] %>% as.character == case_var) == length(dfi[ , exposure_vars[i]] ))
      
      #print("object feeding into cc command:")
      #str(dfi[, c(case_var)] == is_case) %>% print
      #dfi[, exposure_vars[i]] %>% str %>% print
      
      
      table(dfi[, c(case_var)]  |> as.character() == is_case  |> as.vector(), 
              dfi[ , exposure_vars[i]] == is_exposed |> as.vector())  |> print()
      
      
      
      dfi <- na.omit(dfi[, c(case_var, exposure_vars[i])])
      
      or <- cc( dfi[, c(case_var)]  |>  as.character() == is_case |>  as.vector(), 
        dfi[ , exposure_vars[i]] == is_exposed |>  as.vector() ) ## odds ratio
      print("Structure of 'or' object:")
      or  |> str() |> print()
      table_or[[i]] <- ftable(table(dfi[, c(case_var)], dfi[ , exposure_vars[i] ])) |> data.frame()
      names(table_or[[i]]) <- c(case_var,  exposure_vars[i], "Freq")
      
      if (is_case |> is.character()){
        for (j in 1:3) table_or[[i]][,j] <- as.character(table_or[[i]][,j])
      }
      
      
      n_exposed_case[i] <- table_or[[i]][ table_or[[i]][, case_var] == is_case & 
                    table_or[[i]][, exposure_vars[i]] == is_exposed , "Freq"]
      n_unexposed_case[i] <- table_or[[i]][ table_or[[i]][, case_var] == is_case & 
                    table_or[[i]][, exposure_vars[i]] != is_exposed , "Freq"]
      n_exposed_non_case[i] <- table_or[[i]][ table_or[[i]][, case_var] != is_case & 
                    table_or[[i]][, exposure_vars[i]] == is_exposed , "Freq"]
      n_unexposed_non_case[i] <- table_or[[i]][ table_or[[i]][, case_var] != is_case &  
                    table_or[[i]][, exposure_vars[i]] != is_exposed , "Freq"]
      
      print(paste("structure of OR table is:", str(or$table)))
      
      p.value[i] <- ifelse(fisher.test(or$table)$p.value < 0.001, "< 0.001", 
        fisher.test(or$table)$p.value |> threedp())
      #browser()
      tab[[i]] <- c(n_exposed_case[i], n_exposed_non_case[i], n_unexposed_case[i],
                  n_unexposed_non_case[i]) |> as.numeric()
      
      print("Structure of tab[[i]] is:")
      print(tab[[i]])
      ## calculate population attributable risk
      #browser()
      PAR_alt[[i]] <- epiR::epi.2by2(tab[[i]],
                                method="cohort.count",
                                digits=2,
                                conf.level=0.95, 
                                units=100,
                                interpret=TRUE,
                                outcome="as.columns")
      #browser()
      PAR[i] <- paste0((PAR_alt[[i]]$massoc.detail$PAFRisk.strata.wald[[1]]*100) |> onedp(), "% (95% CI: ",
                    (PAR_alt[[i]]$massoc.detail$PAFRisk.strata.wald[[2]]*100) |> onedp(), " to ",
                    (PAR_alt[[i]]$massoc.detail$PAFRisk.strata.wald[[3]]*100) |> onedp(), ")")
      
      print("PAR[i] is: ")
      print(PAR[i])
      
      print(paste("pvalue is: ", p.value[i], sep = ""))
      ret_dfi[[i]] <- data.frame(exposure = exposure_vars[i], odds_ratio = or$or, 
            conf.int = paste("(", or$ci.or[1] |>  twodp(), 
            " to ", or$ci.or[2] |>  twodp(), 
            ")", sep = "" ), P_value = p.value[i], PAR = PAR[i])
      
    }
    n_exposed_case |> print()
    ret_df <- do.call(rbind, ret_dfi)
    #ret_df <- cbind(ret_dfi, n_exposed_case, n_unexposed_case, n_exposed_non_case, n_unexposed_non_case)
    
    #ret_df %>% print
    cases <- cbind(n_exposed_case, n_unexposed_case, n_exposed_non_case, n_unexposed_non_case) |> 
                                                                                        as.data.frame()
    for (i in 1:ncol(cases)) cases[, i ] <- as.numeric(cases[, i ] %>% as.character)
    ret_df <- cbind(ret_df, cases)
    #ret_df |>  print()
    names(ret_df) <- c("Exposure", "Odds ratio", "95% CI","P-value","PAR","Exposed cases",
                       "Unexposed cases", "Exposed non-cases",
                       "Unexposed non-cases")
    
    print("ret_df data.frame")
    print(ret_df)
    
    ret_df$`Prevalence exposure` <- with(ret_df, (`Exposed cases` + 
            `Exposed non-cases`) / (`Exposed cases` +
            `Unexposed cases` + `Exposed non-cases` + `Unexposed non-cases`))
    
    ret_df$`Relative risk` <-   with(ret_df, (`Exposed cases` /
            (`Exposed cases` + `Exposed non-cases`)) /
            (`Unexposed cases` /
            (`Unexposed cases` + `Unexposed non-cases`)))
    
  #  ret_df$`PAR (percent)` <- with( ret_df,  100*((`Prevalence exposure` * (`Relative risk` - 1)) / (1 +
   #         `Prevalence exposure` * (`Relative risk` - 1)))) %>% onedp
    
    #ret_df$P_value <- with(ret_df, )
    ret_df$`PAR (percent)` <- ret_df$PAR
    
    checkTrue(ret_df[1, "Odds ratio"], max(ret_df[, "Odds ratio"] |> as.numeric())) |> print()
    
    ret_df <- ret_df[ order(ret_df[, "Odds ratio"], decreasing = TRUE) , ] 
    
    checkTrue(ret_df[1, "Odds ratio"] == max(as.numeric(ret_df[, "Odds ratio"])))  |>  print()
    
    ret_df <- ret_df[ , c("Exposure", "Exposed cases",  "Exposed non-cases", "Unexposed cases",
                          "Unexposed non-cases", "Odds ratio", "95% CI","P-value", "Relative risk",
                          "Prevalence exposure", "PAR (percent)")]
    
    for (i in 2:5) ret_df[, i] <- ret_df[, i] |> formatC(format = "f", digits = 0)
    ret_df
  }
  #####################################################################################
  ## create table comparing cases to non-cases
  
  table_compare_cases_non_cases_cat <- function(file = input$file1$datapath, header = input$header, 
      sep = input$sep, quote = input$quote, 
      exposure_vars = input$exposure_vars_summ, 
      case_var = input$case_var_summ, is_case = input$is_case_summ){
   
    df <- import_data()
    #df1 <- import_csv(stringsAsFactors = FALSE)
    #df %>% str %>% print
    numeric_vars <- sapply(df[, exposure_vars], is.numeric) %>% as.vector
    
    exposure_vars <- exposure_vars[numeric_vars == FALSE] %>% na.omit %>% as.vector
    #print("exposure_vars post conversion:")
    exposure_vars %>% str %>% print
    
    n_factor <- 1:length(exposure_vars)
    
    for (i in 1:length(exposure_vars)) n_factor[i] <- length(as.factor(
      df[, exposure_vars[i], drop = TRUE ]) %>% levels)
    
    n_row_table <- sum(n_factor)
    
    print(paste("number of table rows are:", n_row_table,sep = ""))
    
    level_name_tab <- 1:length(exposure_vars) %>% as.list
    
    
    for (i in 1:length(exposure_vars)) level_name_tab[[i]] <- levels( df[, exposure_vars[i] ] ) 
    level_name_tab <- do.call(rbind, level_name_tab)
    level_name_tab %>% paste("levels are:", .) %>% print
    
    tab <- 1:n_row_table %>% as.list
    col1 <- 1:n_row_table %>% as.list
    col2 <- 1:n_row_table %>% as.list
    col3 <- 1:n_row_table %>% as.list
    ftab <- 1:n_row_table %>% as.list
    p_values <- 1:length(exposure_vars) %>% as.list
   
    for (i in 1:length(exposure_vars)) {
      
      
    tab[[i]] <- CrossTable(df[, case_var] %>% as.vector,
            df[, exposure_vars[i] ] %>% as.vector) %>% data.frame
    print(tab[[i]])
    
    #n_factor <- 1:(as.factor(df[, exposure_vars[i] ]) %>% levels %>% length)
    
    #levels(as.factor(df[, exposure_vars[i] ])) %>% print
    
   
   col1[[i]] <- c(paste(exposure_vars[i], " is ", 
                        tab[[i]][tab[[i]]$t.x == is_case, "t.y"]   , sep = ""))
   
  
    print(paste("rownames are:", col1[[i]], sep = ""))
    
    
    col2[[i]] <- c(paste(tab[[i]][tab[[i]]$t.x == is_case, "t.Freq"] , " (", 
                (100*tab[[i]][tab[[i]]$t.x == is_case, 
                              "prop.row.Freq"]) %>% onedp, ")", sep = ""))
    
    #print(col2[[i]])
    col3[[i]] <- c(paste(tab[[i]][tab[[i]]$t.x != is_case, "t.Freq"], " (",
                  (100*tab[[i]][tab[[i]]$t.x != is_case, 
                                "prop.row.Freq"]) %>% onedp, ")", sep = ""))
    
    #print("col3[[i]] structure is: ", col3[[i]] %>% str)
    
    
    #p_values %>% print("p_value object is: ", .)
    if ( levels(df[, exposure_vars[i] ]) %>% length <= 1) {
     p_values[[i]] <- "" %>% as.list
    } else {
    p_values[[i]] <- CrossTable(df[, case_var] %>% as.vector,
            df[, exposure_vars[i] ] %>% as.vector, fisher = TRUE)$fisher.ts 
    p_values[[i]] <- p_values[[i]]$p.value
    ## adjust_p_value
    p_values[[i]] <- format_p_value( p_values[[i]] )
    
    }
    
    ## Add missing values to p-value table if required.
    if (levels(  df[, exposure_vars[i] ]) %>% length > 1) {
   p_values[[i]] <- c( p_values[i], 
      rep( "", (levels( df[, exposure_vars[i] ]) %>% length) - 1 )) 
    }
  p_values[[i]] <- do.call(rbind, p_values[[i]] ) %>% as.character
  
    ftab[[i]] <- data.frame(col1[[i]], col2[[i]], col3[[i]], p_values[[i]])
    
    
    ftab[[i]] <- rbind(ftab[[i]], c(rep("", 4)))
    
    names(ftab[[i]]) <- c("Exposure", paste(case_var," is ", is_case, sep = ""), 
        paste(case_var," is ", levels(df[, case_var])[
          which(levels(df[, case_var]) != is_case)], sep = ""), "P-value")
    
    #print(ftab[[i]])
    #ftab[[i]] <- ftab[[i]][ 1:(levels( df[, exposure_vars[i] ]) %>% length) + 1 , ]
    
    
     
    }
    
   # print("ftab class is: ", ftab %>% class)
    
    fftab <- do.call("rbind", ftab) 
    
    #print(fftab)
    #levels(df[, case_var]) %>% print
    
    fftab %<>% na.omit 
    fftab %>% return
    
    
  }
 
  ### table to deal with numeric variables, to be completed.
  table_compare_cases_non_cases_numeric <- function(file = input$file1$datapath, header = input$header, 
      sep = input$sep, quote = input$quote, 
      exposure_vars = input$exposure_vars_summ, 
      case_var = input$case_var_summ, is_case = input$is_case_summ){
    if(is.null(input$file1$datapath) | is.null(input$exposure_vars_summ) | 
          is.null(input$case_var_summ ) ) return()
    df <- import_data()
    numeric_vars <- unlist( lapply(df[, exposure_vars], is.numeric))
    print('numeric variabes are:')
    print(str(numeric_vars))
    name_numeric_vars <- names(numeric_vars == TRUE)
   
    print('names of numeric variables are:')
    print(name_numeric_vars)
    
    stopifnot(is.logical(numeric_vars))
    exposure_vars <- exposure_vars[numeric_vars == TRUE] %>% na.omit %>% as.vector
    print("Exposure variables are:")
    print(exposure_vars)
    #length_num_var <- as.list(1:length(exposure_vars))
    #for (i in 1:length(length_num_var)) length_num_var[[i]] <-  t.test(df[df$exposure_vars, is_case], df[, exposure_vars[i]])
    #print(length_num_var)
     if (length(exposure_vars) == 0) {
      return(NULL)
    } else {
    
    
    l_exp_vars <- 1:length(exposure_vars)
    cases_means <- l_exp_vars
    for (i in 1:length(cases_means)) cases_means[i] <- mean(df[df[, case_var[i]] == is_case, 
      exposure_vars[i]], na.rm = TRUE)
    cases_sds <- l_exp_vars
    for (i in 1:length(cases_sds)) cases_sds[i] <- sd(df[df[, case_var[i]] == is_case,
      exposure_vars[i]], na.rm = TRUE)
    non_cases_means <- l_exp_vars
    for (i in 1:length(non_cases_means)) non_cases_means[i] <- mean(df[df[, case_var[i]] != is_case, 
      exposure_vars[i]], na.rm = TRUE)
    non_cases_sds <- l_exp_vars
    for (i in 1:length(non_cases_sds)) non_cases_sds[i] <- sd(df[df[, case_var[i]] != is_case, 
      exposure_vars[i]], na.rm = TRUE)
    
  p_values <- l_exp_vars  
   for (i in 1:length(cases_means)) p_values[i] <- t.test(df[df[, case_var[i]] == is_case, 
      exposure_vars[i]], df[df[, case_var[i]] != is_case, 
      exposure_vars[i]])$p.value
  
  p_values %<>%  format_p_value
    
#print("P values are: ",  p_values)
#print("Structure of p-value object is: ", p_values %>% str)
    
    case_mean_sd <- paste(cases_means %>% onedp, " (", cases_sds %>% onedp, ")", sep = "")
    non_cases_mean_sd <- paste(non_cases_means %>% onedp, " (", non_cases_sds %>% onedp, ")", sep = "")
    exp_var_labs <- paste(exposure_vars, "; mean (sd)", sep = "")
    #p_values <- as.vector(length_num_var$p.value)
    table_numeric <- cbind(exp_var_labs, case_mean_sd, non_cases_mean_sd, p_values) %>% data.frame %>% return
    print("table of numeric variable, compared by case: ")
    print(table_numeric)
    }
  }
  
  
  
    
  ####################################################################################

  output$contents <- renderTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    
   df <- import_data()
    if (is.null(input$file1)) return()## prevents further error message
    if (input$disp == "head") {
      return(head(df))
    } else {
      return(df)
    }

                            })
  
  output$case_var <- renderUI({
    
    df <- import_data()
    df <- select_binary_vars_from_dataframe(df)
    selectInput("case_var", "Disease variable", df %>% names) 
    
    })
  
  
  output$case_var_summ <- renderUI({
    df <- import_data()
    df <- select_binary_vars_from_dataframe(df)
    selectInput("case_var_summ", "Disease variable", df %>% names)
    })
  
  output$duplicate_vars <- renderUI({
    df <- import_data()
    checkboxGroupInput("duplicate_vars", "Duplicate variables", df %>% names) 
    })
  
  
  
  
  
  output$exposure_vars <- renderUI({
    df <- import_data()
    df <- select_binary_vars_from_dataframe(df)
    pickerInput(
        inputId = "exposure_vars", 
        label = "Select/deselect all options for exposure variables", 
        choices = df %>% names, options = list(`actions-box` = TRUE), 
        multiple = TRUE)
    
    #checkboxGroupInput("exposure_vars", "Exposure variable", df %>% names) 
    })
  
  output$exposure_vars_summ <- renderUI({
    df <- import_data()
    
    checkboxGroupInput("exposure_vars_summ", "Exposure variable", df %>% names) 
    })
  
  
  output$Duplicates_or_not <- renderText({
    df <- import_data()
    df_dups <- df[ , input$duplicate_vars]
    if (df[ duplicated(df_dups), ] %>% nrow > 1) {
    x <-  sprintf( "%d duplicates detected.", df[ duplicated(df_dups), ] %>% nrow )
    }
    if(df[ duplicated(df_dups), ] %>% nrow == 1){
    x <-  print("One duplicate detected.")
    }
  if (df[ duplicated(df_dups), ] %>% nrow == 0) {
   x <- print("No duplicates detected.")
  }
    return(x)
  }) 
  
    output$Duplicates_print <- renderTable({
      df <- import_data()
      df_dups <- df[ , input$duplicate_vars]
      if(df[ duplicated(df_dups),] %>% nrow < 1) {
        return(print("No duplicates"))
      }
      df[ duplicated(df_dups), input$duplicate_vars, drop = FALSE ] 
      })
  
  
  
  output$is_case <- renderUI({
    
    df <- import_data()
    checkTrue(df[, c(input$case_var), drop = FALSE] %>% as.factor %>% levels %>% length == 2) %>% print
    checkTrue(df[, c(input$case_var), drop = FALSE] %>% is.factor ) %>% print
    
    df[, c(input$case_var)] %<>% as.factor %>% print
    
    selectInput("is_case", "Select case", levels(df[, c(input$case_var)] |> as.factor()) )
    #input$is_case %>% str %>% print
    
  })
  
   output$is_exposed <- renderUI({
    
    df <- import_data()
    
    
    
    selectInput("is_exposed", "Select exposed category", 
     levels(df[, c(input$exposure_vars[1])] |> as.factor()) )
  })
  
  
  
  output$is_case_summ <- renderUI({
    df <- import_data()
    checkTrue(df[, c(input$case_var), drop = FALSE] %>% as.factor %>% levels %>% length == 2) %>% print
    checkTrue(df[, c(input$case_var), drop = FALSE] %>% is.factor ) %>% print
    df[, c(input$case_var)] %<>% as.factor %>% print
    selectInput("is_case_summ", "Select case", levels(df[, c(input$case_var)] |> as.factor()) )
    #input$is_case %>% str %>% print
  })
  
  
 
  output$summary_df <- renderPrint({
    validate(
      need(input$file1, "Please select file from local machine (.csv or .xlsx)")
    )
    df <- import_data()
    tab <- CreateTableOne(data = df) 
    tab %>% summary
  })
  
   
  

  output$tabpct <- renderTable( {
    validate(
      need(input$exposure_vars, "Please select disease and exposure variables")
    )
    summary_table_or()
    } ) 
   
  output$onset_time <- renderUI({
    
    df <- import_data()
    selectInput("onset_time", "Date of disease onset", df %>% names) 
  })
  
   output$date_format <- renderUI({
    df <- import_data()
    selectInput("date_format", "Date format", c("dd/mm/yyyy",
      "dd/mm/yy","dd-mm-yyyy", "dd/mm/yyyy hh:mm")) 
  })
   
   
    output$break_hist <- renderUI({
    df <- import_data()
    sliderInput("break_hist", "Number of breaks in histogram", 2, 10, 4) 
  })
   
   
   output$epicurve <- renderPlot({
     
     df <- import_data()
     df$time_var <- df[, input$onset_time] %>% strptime( 
       ifelse(input$date_format == "dd/mm/yyyy",
       "%d/%m/%Y",
       ifelse(input$date_format == "dd/mm/yy", "%d/%m/%y",
        ifelse(input$date_format == "dd-mm-yyyy", "%d-%m-%Y", 
          ifelse(input$date_format == "dd-mm-yy", "%d-%m-%y", 
            ifelse(input$date_format == "dd/mm/yyyy hh:mm", "%d/%m/%Y %H:%M", NA)))))) %>% as.POSIXct
     validate(
       need(df$time_var, "Need valid onset time variable")
     )
     histogram(~ df$time_var, xlab = "Date of onset", type = "count" , nint = input$break_hist)
   })
   
 output$mean_onset_date <- renderText({
   if(!is.numeric.POSIXt(input$onset_time)) return()
   df <- import_data()
      df$time_var <- df[, input$onset_time] %>% strptime( 
        ifelse(input$date_format == "dd/mm/yyyy", "%d/%m/%Y",
            ifelse(input$date_format == "dd/mm/yy", "%d/%m/%y",
            ifelse(input$date_format == "dd-mm-yyyy", "%d-%m-%Y", 
            ifelse(input$date_format == "dd-mm-yy", "%d-%m-%y", 
              ifelse(input$date_format == "dd/mm/yyyy hh:mm", 
                "%d/%m/%Y %H:%M", NA)))))) %>% as.POSIXct
      error <- qt(0.975,df = length(df$time_var) - 1)*
        sd(df$time_var, na.rm = TRUE)/sqrt(length(df$time_var))
      
      left <- mean(df$time_var, na.rm = TRUE) - error
      right <- mean(df$time_var, na.rm = TRUE) + error
     
      print(paste("The mean date of onset is: ", format(mean(df$time_var, na.rm = TRUE), "%d/%b/%Y %H:%M"), ". ",
          "The 95% confidence interval for the mean is: ", 
          format(left,"%d/%b/%Y %H:%M"), " to ", format(right, "%d/%b/%Y %H:%M"), sep = ""))
    
 })
   
 output$compare_characteristics_by_case_status <- renderTable({
   validate(
      need(input$exposure_vars_summ, "Please select disease and exposure variables")
    )
    cat_vars <- table_compare_cases_non_cases_cat()
   #num_vars <- table_compare_cases_non_cases_numeric()
   num_vars <- table_compare_cases_non_cases_numeric()
   if (is.null(num_vars)) {
     cat_vars
   } else {
   names(num_vars) <- names(cat_vars)
   final_tab <- rbind(cat_vars, num_vars)
   final_tab
   }
 })
   
 
 output$missing_data_plot <- renderPlot({
    df <- import_data()
    df <- df[ , order(colSums(is.na(df)), decreasing = TRUE) ]
    vis_miss(df) +
          theme(plot.margin = unit(c(0.5,2,0,0),"inch")) 
    
 })
   
 output$variable_type_plot <- renderPlot({
   df <- import_data()
   df <- df[ order(rowSums(is.na(df)), decreasing = TRUE), ]
   vis_dat(df) %>% plot
 })
 
 # ##########################################################
 # ## srd panel
 # output$case_srd_var <- renderUI({
 #    
 #    df <- import_csv()
 #   
 #    selectInput("case_srd_var", "Disease variable", df %>% names) 
 #    #input$is_case %>% str %>% print
 #    
 #  })
 # 
 # output$exposure_vars_srd <- renderUI({
 #   df <- import_csv()
 #    # Only include binary variables
 #    x <- 1:ncol(df) %>% as.list
 #    print(paste("preliminary version of x: ", x, sep = ""))
 #    for (i in 1:ncol(df) ) x[[i]] <- levels(as.factor(df[, i])) %>% length == 2
 #    x <- do.call(rbind, x) %>% as.vector
 #    print(paste("after version of x: ", x, sep = ""))
 #    names_excl_other_than_2_levels <- names(df)[x]
 #    df <- df[, names(df) %in% names_excl_other_than_2_levels]
 #    df <- df[, which(names(df) != input$case_var) ]
 #    print(paste("names of dataframe with 2 levels and excluding case variable", names(df), sep =""))
 #    
 #    selectInput("exposure_vars_srd", "Exposure variable", names(df)) 
 #  })
 #   
 #  output$is_case_srd <- renderUI({
 #    
 #    df <- import_csv()
 #    checkTrue(df[, c(input$case_srd_var), drop = FALSE] %>% as.factor %>% levels %>% length == 2) %>% print
 #    checkTrue(df[, c(input$case_srd_var), drop = FALSE] %>% is.factor ) %>% print
 #    
 #    df[, c(input$case_srd_var)] %<>% as.factor %>% print
 #    
 #    selectInput("is_case_srd", "Select case", levels(df[, c(input$case_srd_var)] %>% as.factor) )
 #    #input$is_case %>% str %>% print
 #    
 #  })
 # 
 #  output$is_var_srd <- renderUI({
 #    
 #    df <- import_csv()
 #    checkTrue(df[, c(input$exposure_vars_srd), drop = FALSE] %>% as.factor %>% levels %>% length == 2) %>% print
 #    checkTrue(df[, c(input$exposure_vars_srd), drop = FALSE] %>% is.factor ) %>% print
 #    
 #    df[, c(input$exposure_vars_srd)] %<>% as.factor %>% print
 #    
 #    selectInput("is_var_srd", "Select exposed", levels(df[, c(input$exposure_vars_srd)] %>% as.factor) )
 #    #input$is_case %>% str %>% print
 #    
 #  })
 # 
 #  
 #  
 #  output$render_srd <- renderPlot({
 #   df <- import_csv()
 #   df[, input$exposure_vars_srd] <- ifelse(df[, input$exposure_vars_srd] == input$is_var_srd, 1, 0)
 #   df[, input$case_srd_var] <- ifelse(df[, input$case_srd_var] == input$is_case_srd, 1, 0)
 #   srd(df[, c(input$exposure_vars_srd, input$case_srd_var)])
 # })
 # ############################################################
}



shinyApp(ui = ui, server = server)
