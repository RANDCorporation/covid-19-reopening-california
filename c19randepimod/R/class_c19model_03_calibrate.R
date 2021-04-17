

#----------------------------------------------------------------------------------------#
# c19randepimod R package
# This is the R package behind RAND's COVID-19 State policy tool and related papers.
# 
# R Package Author: Pedro Nascimento de Lima
# Model Author: Raffaele Vardavas.
# Copyright (C) 2021 by The RAND Corporation
# See LICENSE.txt and README.txt for information on usage and licensing
#----------------------------------------------------------------------------------------#


#### Calibrate  ----------------------------------------------------------------------####

#----------------------------------------------------------------------------------------#
# Author: Pedro Nascimento de Lima
# Purpose: This File contains functions to calibrate the model.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#



#' Calibrate
#'
#' Calibrates the c19model
#'
#' @param model a model of the c19model class
#' @param location_ids a vector of location ids
#' @param n_lhs the number of samples to use
#' @param minR0 the min R0 that will be used on the calibration process by the set_calibrated_k function
#' @param maxR0 the max R0 that will be used on the calibration process by the set_calibrated_k function
#' @param parallel_mode either FORK or PSOCK. Use PSOCK on windows and FORK on UNIX systems.
#' @param cores number of CPU cores to use.
#'
#' @return a calibrated model of the c19model class
#' @export
calibrate = function(model, location_ids, n_lhs = 2, minR0 = 2, maxR0 = 4, parallel_mode = "FORK", cores, test_run = F, scenarios=NULL) {
  
  # First, set calibration dates and run exponential regressions.
  model = model %>%
    set_calibration_dates(.) %>%
    run_exponential_regressions(.) 
  
  level = model$level
  model_fn = model$model_fn
  model_name = model$model_name
  
  # Determining the Calibratin date as a function of the available data:
  calibration_date = lubridate::as_date(model$calibration_date)
  start_date = lubridate::as_date(model$start_date)
  init_num_calibration_days = as.integer(calibration_date - start_date + 1)
  past_data_calibration_days = as.integer(lubridate::as_date(model$max_ts_date) - start_date + 1)
  
  
  #### Initiation Number Calibration ####
  
  
  # Previously, initiation numbers were calibrated with the baseline parameters.
  # One year into the pandemic, they are set directly because the remote past is no longer as important.
  
  # This section calibrates the model initiation number (e.g. the unknown proportion of people infected when the model starts running.)
  
  # First, Calibrate Initiation Number based using Cumulative Deaths - Do that in Parallel
  # cat_green_tick("Calibrating Initiation Number")
  # 
  # # First, compute cbetas for the reference scenario:
  # model = model %>%
  #   set_time(.,final_time = init_num_calibration_days) %>%
  #   set_calibrated_k(., location_ids = location_ids, n_lhs = 1, minR0 = minR0, maxR0 = maxR0)
  # 
  # # Getting calibration targets:
  # calibration_targets = model$timeseries %>%
  #   filter(LocationID %in% location_ids & Date == calibration_date) %>%
  #   mutate(Target = CumulativeDeaths / Population) %>%
  #   select(LocationID, Target)
  # 
  # if(any(calibration_targets$Target == 0)) {
  #   stop("Calibration targets include zeros. This means that no one is dead at the calibration date. Try getting the inputs outside of the VPN.")
  # }
  # 
  # model$location = model$location %>%
  #   left_join(calibration_targets, by = "LocationID")
  
  # Test Runs are useful to debug the model while it's not running in parallel
  # if(test_run) {
  #   test_results = calibrate_initiation_num(run_id = 1, model = model, calibration_day = init_num_calibration_days)   #calibrate_initiation_num(run_id = 1,, calibration_day = calibration_days)  
  # }
  # 
  # init_num_results = run_calibration_in_parallel(model = model, run_mode = "init.num", parallel_mode = parallel_mode, calibration_day = init_num_calibration_days)
  # 
  # # Then Take the initiation number and run the full LHS for each location:
  # cat_green_tick("Calibrated Initiation Numbers")
  # 
  # model$initiation_numbers = init_num_results %>%
  #   group_by(LocationID) %>%
  #   summarise(InitiationNum = mean(initiation.num)) %>%
  #   left_join(model$location %>% select(LocationID, Name, Population), by = "LocationID") %>%
  #   mutate(InitiallyInfected = Population * InitiationNum)
  
  model$initiation_numbers = model$location %>%
    select(LocationID, Name, Population) %>%
    mutate(InitiationNum = 0.0001) %>%
    mutate(InitiallyInfected = Population * InitiationNum)
  
  cat_green_tick("Running Hypercube for all locations")
  
  model = model %>%
    set_time(.,final_time = past_data_calibration_days) %>%
    set_calibrated_k(., location_ids = location_ids, n_lhs = n_lhs, minR0 = minR0, maxR0 = maxR0, scenarios = scenarios)
  
  # Save Final initial conditions using the stocks, and scenarios:
  
  #test_results = calibrate_params_lhs(run_id = 1, model = model)   #calibrate_initiation_num(run_id = 1,, calibration_day = calibration_days)
  #TODO: Removing calibration_date from lhs_results call.
  #lhs_results = run_calibration_in_parallel(model = model, run_mode = "calibration_lhs",  parallel_mode = parallel_mode, cores = cores, calibration_day = calibration_days)
  
  lhs_results = run_calibration_in_parallel(model = model, run_mode = "calibration_lhs", parallel_mode = parallel_mode, cores = cores)
  
  final_results = lhs_results %>%
    left_join(model$location %>% select(LocationID, LocationShortName), by = "LocationID")
  
  model$status = "calibrated"
  
  model$calibrated_on = Sys.time()
  
  model$cal_results = final_results
  
  model$calibration_date = calibration_date
  
  model$start_date = start_date
  
  model$location_ids = location_ids
  
  check_model_nas(model)
  
  model
}


#' Calibrates the Initiation Number
#'
#' Calibrates the unknown number of infected people at the beginning of the epidemic.
#'
#' @param run_id The RunID (integer)
#' @param model the c19model object
#' @param tolerance percent error to tolerate
#' @param max_runs maximum number of runs to improve initiation number
#' @param initiation.num initial initiation number
#' @param calibration_day day (integer) to use in the process of calibrating the initiation number
#'
#' @return results dataset from the initiation number.
calibrate_initiation_num = function(run_id, model, tolerance = 0.01, max_runs = 10, initiation.num = 1e-4, calibration_day) {
  
  runs = 0
  redo <- TRUE
  
  locationid = model$scenarios$LocationID[run_id]
  
  filtered_inputs = model %>%
    filter_inputs(., location_ids = locationid, level = model$level) %>%
    set_selected_params_for_run_id(.,run_id)
  
  while(redo & runs <= max_runs) {
    
    final_inputs = filtered_inputs %>%
      model$set_initial_stocks(.,initial_exposed = initiation.num) %>%
      model$set_stocks_positions(.)
    
    results = solve_model(final_inputs,  run_id = run_id)
    
    tune.factor = final_inputs$location$Target / results$CumulativeDeaths[calibration_day]
    
    initiation.num = tune.factor * initiation.num
    
    redo = abs(tune.factor-1) > tolerance
    
    runs = runs + 1
    
  }
  
  cat_green_tick(paste0("Calibrated ", filtered_inputs$location$Name, " ini.num after ", runs, " runs. ", round(initiation.num * filtered_inputs$location$Population, 1), " people infected as initiation num."))
  
  results$initiation.num = initiation.num
  
  results$LocationID = locationid
  
  results
  
}

#' Runs a Single Calibration Run and compute fit stats
#'
#' @param run_id The run ID to use
#' @param model a model object of the c19model class
#'
#' @return a results dataframe including fit statistics
calibrate_params_lhs = function(run_id, model, calibration_day) {
  
  locationid = model$scenarios$LocationID[run_id]
  
  # Define the Initiation Number here:
  initiation.num = model$initiation_numbers$InitiationNum[model$initiation_numbers$LocationID == locationid]
  
  final_inputs = model %>%
    filter_inputs(., location_ids = locationid, level = model$level)
  
  input_model = final_inputs %>%
    set_selected_params_for_run_id(.,run_id) %>%
    model$set_initial_stocks(.,initial_exposed = initiation.num) %>%
    model$set_stocks_positions(.) 
  
  results = try(solve_model(model = input_model, run_id = run_id))
  
  # Solve Model
  #results = solve_model(model, run_id = run_id, solver = solver)
  
  # Check if results is not an atomic object:
  if(!(class(results)=="try-error")){
    # Results can be a null object
    
    results$LocationID = locationid
    
    fit_stats = get_ode_fit(model_timeseries = results, actual_timeseries = final_inputs$timeseries)
    
    results$fit.WeightedRMSE = fit_stats$WeightedRMSE
    results$fit.DeathsPercentBias = fit_stats$fit_stats$PercentBias[fit_stats$fit_stats$Outcome == "Deaths"]
    results$fit.DeathsMAE = fit_stats$fit_stats$MAE[fit_stats$fit_stats$Outcome == "Deaths"]
    results$fit.PositiveTestsPercentBias = fit_stats$fit_stats$PercentBias[fit_stats$fit_stats$Outcome == "PositiveTests"]
    results$fit.PositiveTestsMAE = fit_stats$fit_stats$MAE[fit_stats$fit_stats$Outcome == "PositiveTests"]
    
    results$fit.CurrentlyHospitalizedPercentBias = fit_stats$fit_stats$PercentBias[fit_stats$fit_stats$Outcome == "CurrentlyHospitalized"]
    results$fit.CurrentlyHospitalizedMAE = fit_stats$fit_stats$MAE[fit_stats$fit_stats$Outcome == "CurrentlyHospitalized"]
    
    results$fit.DeathsThielsBias = fit_stats$fit_stats$UM_ThielsBias[fit_stats$fit_stats$Outcome == "CurrentlyHospitalized"]
    results$fit.PositiveTestsThielsBias = fit_stats$fit_stats$UM_ThielsBias[fit_stats$fit_stats$Outcome == "CurrentlyHospitalized"]
    results$fit.CurrentlyHospitalizedThielsBias = fit_stats$fit_stats$UM_ThielsBias[fit_stats$fit_stats$Outcome == "CurrentlyHospitalized"]
    
    results
    
  } else {
    return(NULL)
  }
  

}


#' Get ODE fit statistics
#'
#' Compares the ODE results with a set of timeseries. Computes a Weighted Root Mean Squared Error based on pre-specified weights.
#'
#' @param model_timeseries model resulta dataframe
#' @param actual_timeseries actual data dataframe
#' @param model_outcomes vector of outcomes names to be used
#' @param data_outcomes vector of data outcome names to be used
#' @param outcomes_weights weights to be used when computing the weighted RMSE
#' @param time_discount_factor not used for now, but could be used in the future
#'
#' @return a list with fit statistics
#' 
#' @importFrom stats cor 
get_ode_fit = function(model_timeseries, actual_timeseries, model_outcomes = c("Deaths", "CurrentlyHospitalized", "PositiveTests"), data_outcomes = c("DeathsMovingAverage", "CurrentlyHospitalizedMovingAverage", "PositiveTestsMovingAverage"), outcomes_weights = c(1, 0.0001, 0.00001), time_discount_factor = 0.1) {
  
  date_to_filter = max(actual_timeseries$Date)
  #date_to_filter = lubridate::as_date("2020-05-15")
  
  # First, filter the model timeseries:
  
  model_timeseries = model_timeseries %>%
    filter(Date <= date_to_filter)
  
  actual_timeseries = actual_timeseries %>%
    filter(Date <= date_to_filter)
  
  # First, let's verify if the dates match exactly:
  
  check_that(all(model_timeseries$Date == actual_timeseries$Date), msg = "Dates of Actual Timeseries and simulated time series don't match.")
  
  # We care more about fitting the current day than we care abobut the remote past. This can be turned off by setting the discount factor to 0.
  # Here, simulation otucomes are translated to po
  
  sim_outcomes = model_timeseries[,model_outcomes] * actual_timeseries$Population[1]
  
  actual_outcomes = actual_timeseries[,data_outcomes]
  
  error = sim_outcomes - actual_outcomes
  
  weighted_error = error * outcomes_weights
  
  sq_error = error^2
  
  sq_weighed_error = weighted_error^2
  
  mean_model = sapply(sim_outcomes, mean, na.rm=T)
  
  mean_data = sapply(actual_outcomes, mean, na.rm=T)
  
  n = nrow(actual_outcomes)
  
  sd_fn = function(var, data, mean, n) {
    sqrt(sum((data[var] - mean[var])^2)/n)
  }
  
  sd_model = sapply(1:length(sim_outcomes), sd_fn, data = sim_outcomes, mean = mean_model, n = n)
  
  sd_data = sapply(1:length(sim_outcomes), sd_fn, data = actual_outcomes, mean = mean_data, n = n)
  
  corr_fn = function(var, model_data, actual_data, mean_model, mean_data, sd_model, sd_data, n) {
    sum(((model_data[[var]] - mean_model[var]) / sd_model[var]) * ((actual_data[[var]] - mean_data[var]) / sd_data[var])) / n
  }
  
  # This is actual R:
  correlation_coef = sapply(1:length(sim_outcomes), corr_fn, model_data = sim_outcomes, actual_data = actual_outcomes, mean_model = mean_model, mean_data = mean_data, sd_model = sd_model, sd_data = sd_data, n = n)
  
  #
  RSquared = correlation_coef ^ 2
  
  #
  correlation_fn = function(var, model_data, actual_data) {
    cor(model_data[[var]], actual_data[[var]])
  }
  
  correlation = sapply(1:length(sim_outcomes), correlation_fn, model_data = sim_outcomes, actual_data = actual_outcomes)
  
  # Sum of Squared Residuals
  SSR = colSums(sq_error)
  
  # Mean Squared Error
  MSE = SSR / n
  
  # Root Mean Squared Errors
  RMSE = sqrt(MSE)
  
  RMSE_Weighted = sum(RMSE * outcomes_weights)
  
  # Mean Absolute Error
  MAE = colSums(abs(error))/n
  
  MAE_Weighted = sum(MAE * outcomes_weights)
  
  # Mean Absolute Percent Error - Doesnt make sense with all these zeros:
  # MAPE = (colSums(abs(error)/abs(actual_outcomes))) / modcost$var$N
  
  # Thiel Statistics: Morecroft (2007), pg. 399.
  UM_ThielsBias =  ((mean_model - mean_data)^2) / MSE
  
  Bias = mean_model - mean_data
  
  PercentBias = Bias / mean_data
  
  #V Sterman: UM_ThielBiasDiffMeans =  (mean_model^2 - mean_data^2) / MSE
  
  US_ThiellUnequalVariation = ((sd_model - sd_data)^2) / MSE
  #V Sterman: US_ThielUnequalVariation = (sd_model^2 - sd_data^2) / MSE
  
  UC_ThielsUnequalCovariation = (1 / MSE) * (sd_model * sd_data) * (2 * (1 - correlation_coef))
  
  # UM_ThielsBias + US_ThiellUnequalVariation + UC_ThielsUnequalCovariation
  
  stats_fit = list(WeightedRMSE = RMSE_Weighted,
                   fit_stats = data.frame(
                     Outcome = model_outcomes,
                     PercentBias = PercentBias,
                     UM_ThielsBias = UM_ThielsBias,
                     US_ThiellUnequalVariation = US_ThiellUnequalVariation,
                     UC_ThielsUnequalCovariation = UC_ThielsUnequalCovariation,
                     SSR = SSR,
                     MSE = MSE,
                     RMSE = RMSE,
                     MAE = MAE
                   )
  )
  
  stats_fit
  
}
