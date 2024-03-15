

#####################################
### iNZight like plot
### requires mekko library
## 2 category outcome variable
## Many level exposure
# exposure = quoted name of exposure variable in data.frame (must be character or factor variable)
# outcome = quoted name of outcome variable in data.frame (must be character or factor variable) (must be two level)
# event = character value (length 1) of event of interest in outcome variable
# data = data.frame object containing exposure and outcome (not quoted)
# x_axis_label = character string
# y_axis_label = character string
# library(carData)
# library(mekko)
# data("TitanicSurvival")

inzight_plot <- function(exposure = "passengerClass", 
                         outcome = "survived",
                         event = "yes", data = TitanicSurvival, x_axis_label = "Passenger class",
                         y_axis_label = "Proportion survived"){
  # check
  if (!any(names(data) == exposure)) stop("exposure variable not in data")
  if (!any(names(data) == outcome)) stop("outcome variable not in data")
  require(mekko)
  exp_vec <- data[, exposure]
  out_vec <- data[, outcome]
  outcome_levels <- table(out_vec)
  out_cat <- attr(outcome_levels, "dimnames")[[1]] 
  
  # check
  if (!any(out_cat == event)) stop("event argument must be a level of outcome variable") 
  if (!length(out_cat == 2)) stop("outcome variable must be 2 only levels!") 
  
  exposure_levels <- table(exp_vec)
  exp_cat <- attr(exposure_levels, "dimnames")[[1]] 
  prop.table(table(data[exp_vec == 
                          exp_cat[1], outcome]))
    (attr(prop.table(table(data[exp_vec == 
    exp_cat[1], outcome])), "dimnames")[[1]] == "Yes") |> which()
    props_out_by_exp <- 1:length(exp_cat)
  for (i in 1:length(exp_cat)){
    props_out_by_exp[i] <- prop.table(table(data[exp_vec == 
                                                 exp_cat[i], outcome]))[(
  attr(prop.table(table(data[exp_vec == 
          exp_cat[1], outcome])), "dimnames")[[1]] == event) |> which()]
  }
  props_exposure <- prop.table(table(exp_vec))
  props_exposure

  names(props_out_by_exp) <- exp_cat

  names(props_out_by_exp) <- paste0(exp_cat, " (", (props_exposure * 100) |> 
                                       formatC(digits = 1, format = "f"), "%)")
  
  df_cat_plot <- cbind(props_out_by_exp, props_exposure) |> data.frame()
  df_cat_plot$category <- row.names(df_cat_plot)
  mekko::barmekko(df_cat_plot, category, props_out_by_exp, props_exposure)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "none") +
    labs(y= y_axis_label, x = x_axis_label) +
    theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))
}

#inzight_plot() 
