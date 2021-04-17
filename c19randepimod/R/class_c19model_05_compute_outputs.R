

#----------------------------------------------------------------------------------------#
# c19randepimod R package
# This is the R package behind RAND's COVID-19 State policy tool and related papers.
# 
# R Package Author: Pedro Nascimento de Lima
# Model Author: Raffaele Vardavas.
# Copyright (C) 2021 by The RAND Corporation
# See LICENSE.txt and README.txt for information on usage and licensing
#----------------------------------------------------------------------------------------#


#### Compute Outputs -----------------------------------------------------------------####

#----------------------------------------------------------------------------------------#
# Author: Pedro Nascimento de Lima
# Purpose: This File contains functions to compute additional outputs from the model.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#

#' Computes Additional Outputs
#'
#' @param model a c19model object
#' @param compute_econ_outputs TRUE if it should compute additional economic outputs
#' @param calibration TRUE if computing results from calibration
#'
#' @return the model object including augmented outputs
#' @export
compute_augmented_outputs = function(model, compute_econ_outputs = T, calibration = T, ...) {
  
  # First define results I want to extract from the model
  
  if(calibration == TRUE) {
    model_results = model$cal_results
  } else {
    model_results = model$scenarios_results
  }
  
  ## Computing Value of Statistical Life:
  if(FALSE) {
    
    compute_vsl_stats = function(results, model) {
      
      deaths_cols = paste0("D", 1:11)
      # Computing the VSL Lost, for each row of the dataframe.
      results$VSLLost = rowSums(as.matrix(results[,deaths_cols]) %*% diag(model$subpopulation$VSL)) 
      
      results  
      
    }
    
    model_results = model_results %>%
      compute_vsl_stats(results =., model = model)  
  }
  
  
  variables_we_might_want = model$metric$MetricShortName
  
  actual_variables = names(model_results)
  
  variables_to_select = variables_we_might_want[variables_we_might_want %in% actual_variables]
  
  # This is a "flexible" selection, since we try to find variables that we might want:
  augmented_outputs = model_results[c("RunID", "LocationID", "time", "PortfolioID", variables_to_select)] %>%
    mutate(Date = lubridate::as_date(model$start_date) + time - 1) %>%
    left_join(model$location %>% filter(LocationID %in% model$location_ids), by = "LocationID")
  
  
  # WE need number of people to compute the augmented dataset:
  
  augmented_outputs[variables_to_select] = augmented_outputs[,variables_to_select] * augmented_outputs$Population
  
  # Now we can add information from the location table
  augmented_outputs = augmented_outputs %>%
    compute_agregated_stats()
  
  # Again, let's select variables that make sense here, and that are not obviously duplicated:
  
  
  final_variables_to_select = c("RunID", "LocationID", "LocationShortName", "time", "Date", "PortfolioID",variables_we_might_want[variables_we_might_want %in% names(augmented_outputs)])
  
  results = augmented_outputs %>% select(one_of(final_variables_to_select))
  
  ### Computing Economic Outcomes
  if(compute_econ_outputs) {
    
    results = results %>%
      left_join(model$economicinputs %>%
                  filter(EconSensitivity == "B") %>%
                  select(LocationID, PortfolioID, DailyAbsoluteIncomeLoss, DailyPercentIncomeLoss), by = c("LocationID", "PortfolioID"))
    
  }
  
  # Return Model with Computed Results:
  if(calibration == TRUE) {
    model$cali_augm_results = results
  } else {
    model$scenarios_augm_results = results
  }
  
  model
}


#' Get Best Fit Runs
#'
#' This function selects the n best-fitting calibration runs for each location in a calibrated model.
#'
#' @param model A calibrated c19model object
#' @param n The number of runs to get.
#'
#' @return a model object with filtered runs. The cal_results and the cali_augm_results tables are filtered.
#' @export
get_best_fit_model = function(model, n = 1) {
  
  best_fit_runs = model$cal_results %>%
    filter(time == max(time)) %>%
    select(LocationID, RunID, fit.WeightedRMSE) %>%
    arrange(LocationID, fit.WeightedRMSE) %>%
    group_by(LocationID) %>%
    mutate(fit_rank = row_number()) %>%
    filter(fit_rank <= n)
  
  # Saving only the best fit run:
  best_fit_model = model
  
  best_fit_model$cal_results = model$cal_results %>%
    filter(RunID %in% best_fit_runs$RunID)
  
  check_that(is.data.frame(model$cali_augm_results),msg =  "Augmented Calibration results doesnt exist. Make sure to compute_augmented_results.")
  
  best_fit_model$cali_augm_results = model$cali_augm_results %>%
    filter(RunID %in% best_fit_runs$RunID)
  
  best_fit_model$best_fit_runs = best_fit_runs
  
  best_fit_model
  
}

#' Gets Partial Rank Correlation Statistics
#'
#' @param model model object
#' @param location_id A location id
#' @param outcome_variable THe outcome variable to use
#'
#' @return a dataframe with results from the PRCC analysis
#' @export
get_prcc = function(model, location_id, outcome_variable) {
  
  params_with_variation = model$parameter %>%
    filter(Model == model$model_name & MaxCalibrationValue > MinCalibrationValue)
  
  params = params_with_variation$InternalName
  
  prcc_data = calibrated_model$scenarios %>%
    filter(LocationID == location_id) %>%
    left_join(calibrated_model$cali_augm_results %>%
                select(LocationID, outcome_variable, Date, RunID) %>%
                filter(LocationID == location_id & Date == max(Date)), by = c("RunID", "LocationID")) %>%
    ungroup(.) %>%
    select(-Date, -RunID, -LocationID) %>%
    select(one_of(c(params, outcome_variable)))
  
  output = epiR::epi.prcc(dat = prcc_data)
  
  output = output %>%
    mutate(param = params) %>%
    arrange(desc(gamma))
  
  output
  
}

#' Plots scatter plots showing if any of the parameters is driving model bias
#'
#' @param calibrated_model The Calibrated Model
#' @param param parameter name
#' @param fit.stat The fit statistic to display in the plot
#'
#' @return a plot
#' @export
plot_param_fit = function(calibrated_model, param, fit.stat = "fit.DeathsPercentBias") {
  
  calibration_summary = calibrated_model$cal_results %>%
    filter(Date == max(Date)) %>%
    select(RunID, LocationID, LocationShortName, fit.DeathsMAE, fit.DeathsPercentBias, fit.WeightedRMSE, fit.PositiveTestsMAE, fit.PositiveTestsPercentBias, fit.CurrentlyHospitalizedPercentBias, fit.CurrentlyHospitalizedMAE) %>%
    left_join(calibrated_model$scenarios, by = c("RunID", "LocationID", "LocationShortName"))
  
  params_with_variation = calibrated_model$parameter %>%
    filter(Model == calibrated_model$model_name & MaxCalibrationValue > MinCalibrationValue)
  params = params_with_variation$InternalName
  
  long_calibration_summary = calibration_summary %>%
    tidyr::gather(key = param, value = param.value, -RunID, -LocationID, -LocationShortName, -fit.DeathsMAE, -fit.DeathsPercentBias, -fit.WeightedRMSE, -fit.PositiveTestsMAE, -fit.PositiveTestsPercentBias, -fit.CurrentlyHospitalizedPercentBias, -fit.CurrentlyHospitalizedMAE,  -ScenarioID) %>%
    filter(param %in% params)
  
  # From here:
  # https://stackoverflow.com/questions/28436855/change-the-number-of-breaks-using-facet-grid-in-ggplot2
  # s is the scaling factor (cf. multiplicative expand)
  equal_breaks <- function(n = 3, s = 0.05, ...){
    function(x){
      # rescaling
      d <- s * diff(range(x)) / (1+2*s)
      seq(min(x)+d, max(x)-d, length=n)
    }
  }
  
  plot = long_calibration_summary %>%
    ggplot(mapping = aes_string(x = "param.value", y = fit.stat, color = "LocationShortName")) +
    geom_point() +
    ggplot2::geom_hline(yintercept = 0) # +
    #theme_classic()
    
  
  
  if(missing(param)) {
    plot = plot + facet_wrap(~param, scales = "free_x", ) + 
    # use 3 breaks, 
    # use same s as first expand argument, 
    # second expand argument should be 0
    scale_x_discrete(breaks=equal_breaks(n=3, s=0.05), 
                       expand = c(0.05, 0)) + 
      # set the panel margin such that the 
      # axis text does not overlap 
      theme(axis.text.x = element_text(angle=45), 
            panel.margin = unit(1, 'lines'))
  }
  
  plot
  
}