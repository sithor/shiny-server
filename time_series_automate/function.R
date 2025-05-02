evaluate_aic_over_grid <- function(current_data, current_age, current_cp_method, current_data_type, n_points = 15) {
  if (is.null(current_data) || is.null(current_age) ||
      is.null(current_cp_method) || is.null(current_data_type)) {
    return(NULL)
  }
  
  if (current_cp_method != "tscount (interv_detect)") {
    return(NULL)
  }
  
  column_name <- paste0(current_data_type, "_", current_age)
  if (!(column_name %in% names(current_data))) {
    return(NULL)
  }
  
  start_date <- current_data$Year[1] + (current_data$Week[1] - 1) / 52
  time_series <- ts(round(current_data[[column_name]]), frequency = 52, start = start_date)
  regressors <- cbind(linearTrend = seq(along = time_series) / 52)
  
  delta_grid <- seq(0.01, 0.99, length.out = n_points)
  aic_values <- numeric(length(delta_grid))
  
  for (i in seq_along(delta_grid)) {
    delta <- delta_grid[i]
    aic_values[i] <- tryCatch({
      fit <- suppressWarnings(tsglm(ts = time_series,
                                    xreg = regressors,
                                    model = list(past_obs = c(1, 52)),
                                    distr = "nbinom", link = "log",
                                    info = "score"))
      if (is.null(fit)) return(NA_real_)
      
      interv_result <- suppressWarnings(interv_detect(fit, delta = delta, est_interv = TRUE))
      if (is.null(interv_result)) return(NA_real_)
      
      if (interv_result$tau_max == 1 || interv_result$tau_max == length(time_series)) {
        return(NA_real_)
      }
      
      intervention_indicator <- interv_covariate(n = length(time_series),
                                                 tau = interv_result$tau_max,
                                                 delta = delta)
      fit_i <- suppressWarnings(tsglm(ts = time_series,
                                      xreg = cbind(linearTrend = seq(along = time_series) / 52,
                                                   intervention = intervention_indicator),
                                      model = list(past_obs = c(1, 52)),
                                      distr = "nbinom", link = "log",
                                      info = "score"))
      AIC(fit_i)
    }, error = function(e) NA_real_)
  }
  
  return(data.frame(delta = delta_grid, AIC = aic_values))
}