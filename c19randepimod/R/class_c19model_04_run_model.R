

#----------------------------------------------------------------------------------------#
# c19randepimod R package
# This is the R package behind RAND's COVID-19 State policy tool and related papers.
# 
# R Package Author: Pedro Nascimento de Lima
# Model Author: Raffaele Vardavas.
# Copyright (C) 2021 by The RAND Corporation
# See LICENSE.txt and README.txt for information on usage and licensing
#----------------------------------------------------------------------------------------#


#### Run Model  ----------------------------------------------------------------------####

#----------------------------------------------------------------------------------------#
# Author: Pedro Nascimento de Lima
# Purpose: This File contains functions to run the model.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#

#' Solves the c19model
#'
#' @param model a model object of the c19model class
#' @param run_id the run id
#' @param solver lsoda is the preferable option
#'
#' @return a results dataframe
solve_model = function(model, run_id, solver = "lsoda", ...) {
  
  # Use lsoda by default
  if(missing(solver)) {
    solver = "lsoda"
  }
  
  # Here, Run Pre-computed code, with the benefit of not having to run it at
  # every time step. Every parameter-dependent, exogenous computation can
  # be performed at the pre_compute function.
  if(exists('pre_compute', where=model)) {
    # Run the Pre-Compute Function if it exists:
    model = model$pre_compute(model = model, run_id = run_id)
    
  }
  
  # Try Catch block to recover from errors from the solver:
  
  results = tryCatch({
    
    # Try running the model with the solver of choice.
    if(solver == "lsoda") {
      
      results = deSolve::lsoda(y = model$init_stocks,
                               times = model$time,
                               func = model$model_fn,
                               parms = model$params,
                               model = model,
                               run_id = run_id,
                               hmax = 1/2,
                               # Setting an hmin, otherwise simulation is taking too long
                               #hmin = 1/8,
                               # Setting the maxsteps parameter to test if slow runs improve
                               #maxsteps = 500,
                               ...)
      
    } else {
      results = deSolve::ode(y = model$init_stocks,
                             times = model$time,
                             func = model$model_fn,
                             parms = model$params,
                             method = solver,
                             model = model,
                             run_id = run_id,
                             ...)
      
    }
    
    # Filter only exact dates:
    results = results[!results[,1]%%1,]
    # check_that(!any(is.nan(results)), "Error: There are Nan values in your results dataset.")
    results = as.data.frame(results)
    # Defining Dates:
    results$Date = results$time - 1 + model$start_date
    
    return(results)
    
  },
  
  ########################################################################
  # Handling Warnings - Return results:
  ########################################################################
  
  # Handler when a warning occurs:
  # warning = function(cond) {
  #   
  #   cat_red_bullet("deSolve returned a Warning, returning NULL results as they are:")
  #   message(cond)
  #   
  #   # Choose a return value when such a type of condition occurs
  #   return(NULL)
  # },
  
  # Handler when an error occurs:
  error = function(cond) {
    cat_red_bullet("deSolve returned an Error, returning NULL.")
    message(cond)
    # Choose a return value when such a type of condition occurs
    return(NULL)
  },
  
  finally = {
    # Run something after the trycatch
  }
  )
  
  return(results)
}


#' Runs a Single Future Run
#'
#' @param run_id The RunID
#' @param model The Model Object
#'
#' @return the results dataframe
run_single_future_run = function(run_id, model, solver, ...) {
  
  locationid = model$scenarios$LocationID[model$scenarios$RunID == run_id]
  
  #check_that(is.number(locationid), "Location ID was not identified check your run id.")
  #check_that(is.number(run_id), "Location ID was not identified check your run id.")
  
  final_inputs = model %>%
    filter_inputs(., location_ids = locationid, level = model$level) %>%
    set_selected_params_for_run_id(.,run_id) %>%
    model$set_stocks_positions(.)
  
  results = solve_model(final_inputs, run_id = run_id, solver = solver, ...)
  
  results$LocationID = locationid
  
  results
  
}

#' Runs All (Pre-Specified) Scenarios
#'
#' @param model a model object
#' @param portfolio_ids1 a vector of portfolio ids to change to
#' @param location_ids A vector of location ids
#' @param start_dates a vector of dates to use
#' @param duration1 the duration of portfolio 1
#' @param portfolio_ids2 a vector of portfolios to change to
#' @param portfolio_combination_mode use "new_normal"
#' @param uncertainty_mode use "best_fitting", but we could define other types of uncertainty analysis
#' @param uncertainty_runs number of runs to simulate.
#' @param sim_end_date the end date of the simulation (passed as a string aaaa-mm-dd)
#' @param parallel_mode either FORK or PSOCK
#' @param cores number of CPU cores to use.
#'
#' @return a list including the results and the future scenarios simulated.
#' 
#' @import parallel
#' @export
run_all_scenarios = function(model, portfolio_ids1,
                             location_ids,
                             start_dates,
                             duration1,
                             portfolio_ids2,
                             portfolio_combination_mode,
                             uncertainty_mode,
                             uncertainty_runs,
                             sim_end_date,
                             parallel_mode,
                             cores,
                             solver, test_run = F, ...) {
  
  # These steps are taken for every state and don't need to be done in parallel:
  
  # Specify which calibrated runs we will use
  
  # First, selecting runs to use:
  if(uncertainty_mode == "best_fitting") {
    selected_model = get_best_fit_model(model = model, n = uncertainty_runs)
    
  } else {
    check_that(FALSE, msg = "Choose a proper uncertainty mode.")
  }
  
  
  intervention_dates = data.frame(Portfolio1StartDate = start_dates, stringsAsFactors = F) %>%
    mutate(Portfolio1StartDateID = row_number())
  
  # Determine Run Combinations:
  if(portfolio_combination_mode == "all_combinations") {
    
    
    future_runs = expand.grid(location_ids, portfolio_ids1, intervention_dates$Portfolio1StartDateID, duration1, portfolio_ids2, stringsAsFactors = F)
    
    names(future_runs) = c("LocationID", "Portfolio1ID", "Portfolio1StartDateID", "Portfolio1Duration", "Portfolio2ID")
    
    
  } else if(portfolio_combination_mode == "single_portfolio") {
    
    future_runs = expand.grid(location_ids, portfolio_ids1, intervention_dates$Portfolio1StartDateID, duration1, stringsAsFactors = F)
    names(future_runs) = c("LocationID", "Portfolio1ID", "Portfolio1StartDateID", "Portfolio1Duration")
    future_runs$Portfolio2ID = future_runs$Portfolio1ID
    
  } else if(portfolio_combination_mode == "new_normal") {
    
    
    portfolio_combinations = data.frame(Portfolio1ID = portfolio_ids1, Portfolio2ID = portfolio_ids2, stringsAsFactors = F)
    
    future_runs = expand.grid(location_ids, portfolio_combinations$Portfolio1ID, intervention_dates$Portfolio1StartDateID, duration1, stringsAsFactors = F)
    names(future_runs) = c("LocationID", "Portfolio1ID", "Portfolio1StartDateID", "Portfolio1Duration")
    
    future_runs = future_runs %>%
      left_join(portfolio_combinations, by = "Portfolio1ID")
    
  } else {
    check_that(FALSE, msg = "Choose a proper Portfolio combination mode.")
  }
  
  # Making sure nothing is a factor:
  future_runs = droplevels(future_runs)
  
  intervention_dates = droplevels(intervention_dates)
  
  future_runs = future_runs %>%
    left_join(intervention_dates, by = "Portfolio1StartDateID")
  
  
  
  # Setting up future runs that will become inputs to the run single future run.
  future_runs = future_runs %>%
    mutate(FutureScenarioID = row_number(),
           Portfolio1StartDate = lubridate::as_date(Portfolio1StartDate),
           Portfolio2StartDate = Portfolio1StartDate + Portfolio1Duration,
           # The third portfolio is not used in the pre-ran scenarios, so we just set it equal to the second portfolio.
           Portfolio3ID = Portfolio2ID,
           Portfolio3StartDate = Portfolio2StartDate + 1
    )
  
  # Setting up the future runs data.frame. Starting with the selected runs:
  selected_runs = unique(selected_model$cal_results[,c("LocationID","RunID")]) %>% as.data.frame(.) %>%
    filter(LocationID %in% location_ids)
  
  # A Full Join creates all the required runs by combining all selected runs from the calibration process with all future scenarios.
  full_future_runs = selected_runs %>%
    full_join(future_runs, by = "LocationID") %>%
    mutate(FutureScenarioRepID = row_number())
  
  selected_model$future_runs = full_future_runs
  

  
  ### Before running things in parallel, filter the model results so its calibration results are small. This could speed thing up:
  ## Potentially, this could be done at another point.
  filtered_model = selected_model
  
  filtered_model$cal_results = filtered_model$cal_results %>%
    filter(Date == max(Date))
  
  filtered_model$cali_augm_results = filtered_model$cali_augm_results %>%
    filter(Date == max(Date))
  
  ## Now we run all scenarios in parallel:
  if(missing(cores)) {
    no_cores <- detectCores() - 1
  } else {
    no_cores <- cores
  }
  
  
  if(test_run) {
    cat_green_tick(paste(Sys.time(),"- Hold Tight. Testing First Run"))
    
    test_run = run_future_scenario_rep(1, filtered_model, sim_end_date)
  }
  
  start_time = Sys.time()
  
  cat_green_tick(paste(Sys.time(),"- Hold Tight. Running at least", nrow(full_future_runs), "future scenario runs in Parallel with", no_cores, "cores."))
  
  cat_green_tick(start_time)
  
  cl <- makeCluster(no_cores, type = parallel_mode)
  
  set_up_cluster(cluster = cl, model = filtered_model, parallel_mode = parallel_mode)
  
  # Original Apply:
  #scenarios_outputs <- parLapply(cl = cl, X = selected_model$future_runs$FutureScenarioRepID,fun = run_future_scenario_rep, selected_model = filtered_model, sim_end_date = sim_end_date, solver = solver)
  scenarios_outputs <- pbapply::pblapply(cl = cl, X = selected_model$future_runs$FutureScenarioRepID,FUN = run_future_scenario_rep, selected_model = filtered_model, sim_end_date = sim_end_date, solver = solver)
  
  
  stopCluster(cl)
  
  finish_time = Sys.time()
  
  cat_green_tick(finish_time)
  cat_green_tick(paste0(Sys.time(), " - We're done with this simulation. Total time: ", finish_time - start_time, " for ", nrow(full_future_runs), " model runs."))
  
  # Return all runs:
  scenarios_results = do.call(rbind, scenarios_outputs)
  
  # Comput
  
  list(scenarios_results = scenarios_results,
       future_runs = full_future_runs
  )
  
}


# This function is used within the run_all_scenarios to run a specific future scenario run.

run_future_scenario_rep = function(future_rep_id, selected_model, sim_end_date, solver, ...) {
  
  future_run = selected_model$future_runs %>%
    filter(FutureScenarioRepID == future_rep_id)
  
  model_result = simulate_single_scenario(model = selected_model,
                                          state_id = future_run$LocationID,
                                          run_id = future_run$RunID,
                                          sim_end_date = sim_end_date,
                                          npi1 = future_run$Portfolio1ID,
                                          npi2 = future_run$Portfolio2ID,
                                          npi3 = future_run$Portfolio3ID,
                                          date1 = future_run$Portfolio1StartDate,
                                          date2 = future_run$Portfolio2StartDate,
                                          date3 = future_run$Portfolio3StartDate,
                                          solver = solver, ...)
  
  scenario_results = model_result$scenarios_augm_results
  scenario_results$FutureScenarioID = future_run$FutureScenarioID
  scenario_results$FutureScenarioRepID = future_rep_id
  
  scenario_results
  
}

# Runs all defined scenarios in parallel:
#' Runs calibration runs in parallel
#'
#' @param model The model object
#' @param run_mode either "scenarios", "init.num" or "calibration_lhs". We start the calibration process with init.num, then move to "calibration_lhs" and then "scenarios".
#' @param cores number of CPU cores to use.
#' @param parallel_mode either "FORK" or "FORK". "PSOCK" works on windows machines
#' @param ... Additional parameters to be passed to each model run function.
#'
#' @return A data.frame with simulation results.
#' 
#' @import parallel
#' 
run_calibration_in_parallel = function(model, run_mode = "scenarios", parallel_mode, cores, ...) {
  
  if(missing(cores)) {
    no_cores <- detectCores() - 1
  } else {
    no_cores <- cores
  }
  
  start_time = Sys.time()
  
  # Inicializar Cluster
  cat_green_tick(paste("Hold Tight. Running at least", nrow(model$scenarios), " calibration runs in Parallel with ", no_cores, " cores."))
  
  cat_green_tick(start_time)
  
  cl <- makeCluster(no_cores, type = parallel_mode)
  
  set_up_cluster(cluster = cl, model = model, parallel_mode = parallel_mode)
  
  if(run_mode == "init.num") {
    # Original
    #scenarios_outputs <- parLapply(cl = cl,X = 1:nrow(model$scenarios),fun = calibrate_initiation_num, model = model, ...)
    scenarios_outputs <- pbapply::pblapply(cl = cl,X = 1:nrow(model$scenarios),FUN = calibrate_initiation_num, model = model, ...)
  }
  
  if(run_mode == "calibration_lhs") {
    # Original
    #scenarios_outputs <- parLapply(cl = cl,X = 1:nrow(model$scenarios),fun = calibrate_params_lhs, model = model, ...)
    scenarios_outputs <- pbapply::pblapply(cl = cl,X = 1:nrow(model$scenarios),FUN = calibrate_params_lhs, model = model, ...)
    
  }
  
  if(run_mode == "scenarios") {
    # Original
    #scenarios_outputs <- parLapply(cl = cl,X = 1:nrow(model$scenarios),fun = run_single_run, model = model, ...)
    scenarios_outputs <- pbapply::pbapply(cl = cl,X = 1:nrow(model$scenarios),FUN = run_single_run, model = model, ...)
  }
  
  stopCluster(cl)
  
  finish_time = Sys.time()
  
  cat_green_tick(finish_time)
  cat_green_tick(paste0("We're done with this simulation. Total time: ", finish_time - start_time, "for ", nrow(model$scenarios), " model runs."))
  
  do.call(rbind, scenarios_outputs)
  
}


#' Setup Cluster for Parallel Simulations
#'
#' Exports relevant objects in case we are using a FORK parallel cluster.
#'
#' @param cluster A cluster object created with makeCluster
#' @param model the model objecto to use
#' @param parallel_mode Either "FORK" or "PSOCK"
#' @export
set_up_cluster = function(cluster, model, parallel_mode) {
  
  ## Definitive fix to this function will be to export custommodel functions, and if this is a custom Run, run a different clusterExport function.
  
  #model_fn = model$model_fn
  
  if(parallel_mode == "PSOCK") {
    clusterEvalQ(cluster, library(deSolve))
    clusterEvalQ(cluster, library(dplyr))
    clusterEvalQ(cluster, library(c19randepimod))
    # Exportando objetos que preciso ter nos clusters:
    clusterExport(cluster, varlist = list("model",
                                          #"model_fn",
                                          "filter_inputs",
                                          "solve_model",
                                          "safe_division",
                                          "get_ode_fit",
                                          "uniformize_inputs_level",
                                          "run_experiment",
                                          "get_current_intervention_id",
                                          "run_future_scenario_rep",
                                          "run_single_future_run",
                                          "compute_augmented_outputs",
                                          "compute_agregated_stats",
                                          #"set_calibrated_stocks",
                                          #"set_calibrated_stocks.c19model_amshi2s",
                                          #"set_calibrated_stocks.c19model_amshi3s",
                                          #"set_initial_stocks.c19model_amshi2s",
                                          #"set_initial_stocks.c19model_amshi3s",
                                          #"set_initial_stocks",
                                          #"set_computed_params",
                                          #"set_computed_params.c19model_amshi2s",
                                          #"set_computed_params.c19model_amshi3s",
                                          #"model_function",
                                          #"model_function.c19model_amshi2s",
                                          #"model_function.c19model_amshi3s",
                                          #"set_stocks_positions",
                                          #"set_stocks_positions.c19model",
                                          #"set_stocks_positions.c19model_amshi2s",
                                          #"set_stocks_positions.c19model_amshi3s",
                                          "set_selected_params_for_run_id",
                                          "simulate_single_scenario",
                                          "calibrate_initiation_num"), envir = environment())
  }
  
}


print.c19model = function(model) {
  
  cat("### COVID-19 Model ### \n")
  cat("Model Name: ", model$model_name, "\n")
  cat("Model Level: ", model$level, "\n")
  cat("Status: ", model$status, "\n")
  
  if(model$status == "calibrated") {
    cat("Calibrated On: ", as.character(model$calibrated_on), "\n")
    cat("Calibrated for: ", model$location$LocationShortName[model$location$LocationID %in% model$location_ids], "\n")
  }
  
}

# Simulate Single Scenario:

#' Simulates Single Scenario Building an intervention schedule
#'
#' @param model the model object
#' @param state_id location id
#' @param run_id run_id to use
#' @param sim_end_date when to stop simulation
#' @param npi1 first NPI
#' @param npi2 second NPI
#' @param npi3 third NPI
#' @param date1 date to transition to the first NPI
#' @param date2 date to transition to the second NPI
#' @param date3 date to transition to the third NPI
#'
#' @return model object containing simulation results
#' @export
#' 
#' @importFrom utils head
simulate_single_scenario = function(model, state_id, run_id, sim_end_date = "2020-12-31",  npi1 = 6, npi2 = 6, npi3 = 1, date1 = "2020-05-01", date2 = "2020-06-01", date3 = "2020-07-01", solver = "lsoda", ...) {
  
  ## Model State Id should be in the calibrated model:
  #check_that(state_id %in% model$location_ids, "You didn't calibrate the model for this state. Please choose another state.")
  
  current_npi_intervention = get_current_intervention_id(model, location_id = state_id)
  
  intervention_schedule = data.frame(
    SimulatedPortfolioID = c(npi1, npi2 , npi3),
    ChangeDates = c(date1, date2, date3), stringsAsFactors = F
  )
  
  #print(intervention_schedule)
  
  intervention_schedule$ChangeDates = lubridate::as_date(intervention_schedule$ChangeDates)
  
  # Determining the run_id:
  # This part will be done for each run_id, and should be done in parallel for many runs:
  # If Run ID is missing, use the single best fit:
  
  if(missing(run_id)) {
    best_fit_model = get_best_fit_model(model = model, 1)
    run_id = head(model$cal_results$RunID[model$cal_results$LocationID == state_id], 1)
  }
  
  # Set Stocks
  scenarios_model = model %>%
    model$set_calibrated_stocks(., run_id)
  
  scenarios_model$intervention_schedule = intervention_schedule
  
  # Set Time - Based on Initial Date:
  final_date = lubridate::as_date(sim_end_date)
  final_time = max(model$time) + as.integer(final_date - max(model$cal_results$Date))
  scenarios_model$time = (max(model$time)):final_time
  
  scenarios_results = run_single_future_run(scenarios_model, run_id = run_id, solver, ...)
  
  scenarios_model$scenarios_results = scenarios_results
  
  scenarios_model = scenarios_model %>% 
    compute_augmented_outputs(model = ., compute_econ_outputs = T, calibration = F, ...)
  
  scenarios_model
  
}