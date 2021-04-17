

#----------------------------------------------------------------------------------------#
# c19randepimod R package
# This is the R package behind RAND's COVID-19 State policy tool and related papers.
# 
# R Package Author: Pedro Nascimento de Lima
# Model Author: Raffaele Vardavas.
# Copyright (C) 2021 by The RAND Corporation
# See LICENSE.txt and README.txt for information on usage and licensing
#----------------------------------------------------------------------------------------#


#### Run Experiments Functions  ------------------------------------------------------####

#----------------------------------------------------------------------------------------#
# Author: Pedro Nascimento de Lima
#
# Purpose: Generic Functions for Running Experiments
# These functions are used to set and run what-if scenarios defined in a Latin Hypercube
# Sample and/or in a Full Factorial Design Sample.
# These functions are particularly useful for RDM analyses that explore the robustness of
# Strategies across an ensemble of future states of the world.
#
# These functions can be used in parallel in a single machine, and also can be used 
# across several machines to run hundreds, thousands or millions of scenarios.
#
# Creation Date: July 2020
#----------------------------------------------------------------------------------------#

#' Set Parameter
#' 
#' This functions constructs the experimental_parameter object, and appends experimental parameters that will be visible inside the model in the future.
#' Experimental parameters can be either uncertainties or decision levers.
#' Every parameter defined in this function can be accessed within the model by using \code{experimental_parameters$param_name}.
#' 
#' Experimental Parameters are not visible during calibration.
#'
#' @param model the c19model object that has already been calibrated.
#' @param parameter_name charachter string defining the parameter name.
#' @param experimental_design Either "grid" or "lhs" Use lhs if you want to create a Latin Hypercube Sample within the min and max bounds you provided. Use Grid
#' @param values use when experimental_design = "grid". This should be a numeric vector including the values to be included in a grid experimental design.
#' @param min use when experimental_design = "lhs". This should bea numeric value indicating the minimum bound in the Latin Hypercube sample.
#' @param max use when experimental_design = "lhs". This should bea numeric value indicating the minimum bound in the Latin Hypercube sample.
#'
#' @return a c19 model object including the defined experimental parameters.
#' @export
set_parameter = function(model, parameter_name, experimental_design, values, min, max) {
  
  # Checking if Parameters make sense.
  check_that(!missing(model), msg = "A c19model should be provided.")
  
  check_that("c19model" %in% class(model), msg = "The model should be a c19model object.")
  
  check_that(is.character(parameter_name), msg = "Parameter Name should be a character string.")
  
  check_that(experimental_design %in% c("lhs", "grid"), msg = "Experimental Design Should be either lhs or grid.")
  
  if(experimental_design == "lhs")  {
    
    check_that(!missing(min) & !missing(max), msg = "min and max parameters should be provided in an lhs sample.")
    
    check_that(missing(values), msg = "Values parameter should not be provided for an LHS sample.")
    
    check_that(min < max, msg = "Minimum value should lower than the maximum value. Use grid to simulate a single value.")
  
    } else if (experimental_design == "grid") {
    
    check_that(missing(min) & missing(max), msg = "min and max parameters should not be provided in a grid sample.")
    
    check_that(!missing(values), msg = "Values parameter should be provided in a grid experimental design.")
    
  }
  
  # If experimental parameters object doesn't exist, create it:
  if(is.null(model$experimental_parameters)) {
    model$experimental_parameters = list()
  }
  
  # The experimental design object is created here as a list aiming to allow us to pass any length of values in parameters that have a "grid" experimental design.
  if(experimental_design == "lhs") {
    
    model$experimental_parameters[[parameter_name]] = list(parameter_name = parameter_name,
                                                           experimental_design = experimental_design,
                                                           min = min,
                                                           max = max)
    
  }
  
  if(experimental_design == "grid") {
    model$experimental_parameters[[parameter_name]] = list(parameter_name = parameter_name,
                                                           experimental_design = experimental_design,
                                                           values = values
                                                           )
    
  }
  
  return(model)
  
}



#' Set Experimental Design
#' 
#' Creates the future_experimental_design data.frame based on the paramers defined by the set_parameter functions. The experimental design created by this function is useful to run a typical RDM analysis where each policy is evaluated across a LHS of deep uncertainties. To achieve that, define each policy lever as a grid parameter, and each uncertainty as an "lhs" uncertainty.
#'
#' @param model The c19model object
#' @param n_new_lhs The number of points in the Latin Hypercube Sample to be created.
#'
#' @return The model object including a future_experimental_design data.frame.
#' @export
#' @importFrom lhs randomLHS
#' @import dplyr
set_experimental_design = function(model, n_new_lhs, convert_lhs_to_grid = F, lhs_to_grid_midpoints = 0) {
  
  # First step, convert LHS parameters to grid if requested:
  if(convert_lhs_to_grid) {
    
    convert_lhs_param_to_grid  = function(parameter, lhs_to_grid_midpoints) {
      if(parameter$experimental_design == "lhs"){
        parameter$experimental_design = "grid_lhs"
        parameter$values = seq.default(from = parameter$min, to = parameter$max, length.out = 2+lhs_to_grid_midpoints)
        # Clearing min and max values after using them
        parameter$min = NULL
        parameter$max = NULL
      }
      parameter
    }
    
    # Convert all lhs parameters to grid
    model$experimental_parameters = lapply(X = model$experimental_parameters, FUN = convert_lhs_param_to_grid, lhs_to_grid_midpoints = lhs_to_grid_midpoints)
    
  }
    
  
  ## Getting a Data.Frame of LHS Parameters
  lhs_params = Filter(f = function(x) x$experimental_design == "lhs", model$experimental_parameters) %>%
    do.call(rbind.data.frame, .)
  
  grid_lhs_params = Filter(f = function(x) x$experimental_design == "grid_lhs", model$experimental_parameters) %>%
    sapply(., function(x) x[3]) %>%
    expand.grid(.)
  
  # Only sample lhs if there is one LHS variable:
  
  if(nrow(lhs_params)>0) {
    lhs_sample <- lhs::randomLHS(n = n_new_lhs, k = nrow(lhs_params)) %>%
      as.data.frame()
    
    names(lhs_sample) = lhs_params$parameter_name
    
    lhs_experiments = list()
    
    for (param in names(lhs_sample)) {
      ### Here: Also could consider other distributions.
      lhs_experiments[[param]] = qunif(p = lhs_sample[,param], min = lhs_params$min[lhs_params$parameter_name == param], max = lhs_params$max[lhs_params$parameter_name == param])  
    }
    
    lhs_experiments = lhs_experiments %>% as.data.frame(.) %>%
      mutate(LHSExperimentID = row_number())  
    
  } else if(nrow(grid_lhs_params)>0) {
    # create grid with "Lhs parameters", but using a grid desing. Useful to distinguish uncertainties from policies:
    lhs_experiments = grid_lhs_params %>%
      mutate(LHSExperimentID = row_number())
    
    names(lhs_experiments) = sub(pattern = '.values',replacement =  '',x = names(lhs_experiments))
    
  } else {
    # lhs Experiments is a single experiment with no variable:
    lhs_experiments = data.frame(LHSExperimentID = 1)
  }
  
  ## Getting a Data.Frame of Grid Parameters:
  grid_params = Filter(f = function(x) x$experimental_design == "grid", model$experimental_parameters) %>%
    sapply(., function(x) x[3]) %>%
    expand.grid(.)
  
  # If there are no grid parameters, then there's only one point in the grid.
  if(nrow(grid_params)>0){
    grid_params = grid_params %>%
    mutate(GridExperimentID = row_number())
  } else {
    grid_params = data.frame(GridExperimentID = 1)
  }
  
  # Getting Rid of the .values appendix
  names(grid_params) = sub(pattern = '.values',replacement =  '',x = names(grid_params))
  
  # Continue Here -> Create IDs for each table, expand grid and prepare the experimental design table.
  RunIDs = unique(model$cal_results$RunID)
  
  #
  experimental_design = expand.grid(model$location_ids, RunIDs, lhs_experiments$LHSExperimentID, grid_params$GridExperimentID)
  names(experimental_design) = c("LocationID", "RunID", "LHSExperimentID", "GridExperimentID")
  
  # Assert that the Names of Alternative tables don't collide.
  
  all_collumns = c(names(model$scenarios), names(lhs_experiments), names(grid_params))
  
  duplicated_names = all_collumns[duplicated(all_collumns)]
  check_that({
    length(duplicated_names)==0  
  }, msg = paste0("The Names of these Parameters are duplicated: ", duplicated_names))
  
  # Defining the Future Runs:
  future_runs = expand.grid(grid_params$GridExperimentID, lhs_experiments$LHSExperimentID, model$location_ids)
  names(future_runs) = c("GridExperimentID", "LHSExperimentID", "LocationID")
  
  future_runs = future_runs %>%
    left_join(grid_params, by = "GridExperimentID") %>%
    left_join(lhs_experiments, by = "LHSExperimentID")
  
  # Selected Calibrated Runs:
  selected_runs = unique(model$cal_results[,c("LocationID","RunID")]) %>% as.data.frame(.) 
  
  full_future_runs = selected_runs %>%
    full_join(future_runs, by = "LocationID") %>%
    mutate(ExperimentID = row_number())
  
  model$future_experimental_design = full_future_runs
  
  return(model)
  
}


#' Evaluate Experiments (in Parallel)
#' 
#' This function evaluates experiments in parallel, and can be used to scale model runs vertically and horizontally. You can first define your experiments using the \code{set_parameter} and \code{set_experimental_design} functions. Then, you can save your model object and send it to different servers, if you want. Then, use this function to evaluate the experiments in parallel, while saving results to the ./outputs folder. These results can be visualized before your complete experiments are ready.
#'
#' @param model The c19model object
#' @param sim_end_date Simulation end date (in the yyyy-mm-dd format, e.g." "2021-01-20")
#' @param runs Vector of Runs to perform (should correspond to ExperimentIDs defined in the future_experimental_design data.frame)
#' @param n_cores Number of CPU cores to use, defaults to total number of cores - 2
#' @param parallel_mode Parallel Cluster type to use. Either "PSOCK" or "FORK". Windows users, use "PSOCK", otherwise, use "FORK"
#' @param solver "lsoda" by default, but could use any deSolve-compatible solver.
#' @param write_csv TRUE if you want to save preliminary results to an ./output folder. Make sure this folder exists and is empty.
#'
#' @return a data.frame with simulation results
#' @export
evaluate_experiment = function(model, sim_end_date, runs = 1:nrow(model$future_experimental_design), n_cores = parallel::detectCores() - 2, parallel_mode = "PSOCK", solver = "lsoda", write_csv = F, test_run = F) {
  
  # If runs is missing, assume all runs:
  
  ### Reducing the Model Object Size - This is Important to fully leverage parallelization ###
  model$cal_results = model$cal_results %>%
    filter(Date == max(Date))
  
  model$cali_augm_results = model$cali_augm_results %>%
    filter(Date == max(Date))
  
  model$timeseriesnas = NULL
  
  # Selecting only the latest date in the time series object:
  model$timeseries = model$timeseries %>%
    select(LocationID, Date, PositiveTests, PortfolioID, Population, MaxPortfolioID) %>%
    filter(Date == max(Date))
  
  
  cat_green_tick(paste(Sys.time(),"- Hold Tight. Running ", length(runs), "future scenario runs in Parallel with", n_cores, "cores."))
  start_time = Sys.time()
  cat_green_tick(start_time)
  
  if(write_csv) {
    write.csv(x = model$future_experimental_design,file = "./output/experimental_design.csv", row.names = F)
    write.csv(x = model$scenarios, file = "./output/scenarios.csv", row.names = F)
    write.csv(x = model$location, file = "./output/location.csv", row.names = F)
  }
  
  if(test_run) {
    cat_green_tick(paste0(Sys.time(), " - Starting Test Run."))
    scenarios_outputs = run_experiment(experiment_id = runs[1], model = model, sim_end_date = sim_end_date, solver = solver, write_csv = write_csv)  
  }
  
  cl <- parallel::makeCluster(n_cores, type = parallel_mode)
  set_up_cluster(cluster = cl, model = model, parallel_mode = parallel_mode)
  # Original Apply:
  #scenarios_outputs <- parLapply(cl = cl, X = selected_model$future_runs$ExperimentID,fun = run_future_scenario_rep, selected_model = filtered_model, sim_end_date = sim_end_date, solver = solver)
  # Bcat_green_tick(paste(Sys.time(),"- Running Test experiment."))
  
  # Parallel Implementation:
  # Testing a Single Experiment:
  
  scenarios_outputs <- pbapply::pblapply(cl = cl, X = runs,FUN = run_experiment, model = model, sim_end_date = sim_end_date, solver = solver, write_csv = write_csv)
  
  stopCluster(cl)
  
  finish_time = Sys.time()
  
  cat_green_tick(finish_time)
  
  cat_green_tick(paste0(Sys.time(), " - We're done with this simulation. Total time: ", finish_time - start_time, " for ", length(runs), " model runs."))
  
  # Return all runs:
  do.call(rbind, scenarios_outputs)
  
}


#' Runs a Single Experiment
#' 
#' Runs a single Experiment defined by the set_experimental_design function. The experiment_id corresponds to the ExperimentID collumn in the future_experimental_design data.frame
#'
#' @param experiment_id The ExperimentID
#' @param model The c19model object
#' @param sim_end_date Simulation end date (in the yyyy-mm-dd format, e.g." "2021-01-20")
#' @param solver "lsoda" by default, but could use any deSolve-compatible solver.
#' @param ... Additional parameters to be passed to the inner function.
#'
#' @return data.frame with simulation results.
#' @export
run_experiment = function(experiment_id, model, sim_end_date, solver = "lsoda", write_csv) {
  
  future_run = model$future_experimental_design %>%
    dplyr::filter(ExperimentID == experiment_id)
  
  run_id = future_run$RunID
  
  # Set Stocks
  model = model %>%
    model$set_calibrated_stocks(., run_id)
  
  
  # Set Time - Based on Initial Date:
  final_date = lubridate::as_date(sim_end_date)
  final_time = max(model$time) + as.integer(final_date - max(model$cal_results$Date))
  model$time = (max(model$time)):final_time
  
  #scenarios_results = run_single_future_run(scenarios_model, run_id = run_id, solver, ...)
  
  locationid = model$scenarios$LocationID[model$scenarios$RunID == run_id]
  
  # Set Selected Params and Stock Positions:
  model = model %>%
    filter_inputs(., location_ids = locationid, level = model$level) %>%
    set_selected_params_for_run_id(.,run_id) %>%
    model$set_stocks_positions(.)
  
  # Bring experimental parameters to the parameter set:
  model$params = cbind(model$params, future_run)
  
  results = try(solve_model(model, run_id = run_id, solver = solver))
  
  # Solve Model
  #results = solve_model(model, run_id = run_id, solver = solver)
  
  # Check if results is not an atomic object:
  if(!(class(results)=="try-error")){
    # Results can be a null object
    
    results$LocationID = locationid
    results$ExperimentID = experiment_id
    
    # This function executes an user-defined function to process the results. This will replace the "compute augmented outputs" function which is used elsewhere.
    # This allows more flexibility and allows us to tailor the output files for each application we want to have.
    results = model$compute_experiment_output(results = results, model = model)
    
    if(write_csv) {
      # Trying to generate results as we go, within each core:
      #write.csv(x = results, file = "./output/experimental_results.csv", append = T, row.names = F)
      write.table(x = results, file = "./output/experimental_results.csv", append = T, row.names = F, sep = ",")
    }
    
    results
    
  } else {
    return(NULL)
  }
  
 
  
}

