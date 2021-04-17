

#----------------------------------------------------------------------------------------#
# c19randepimod R package
# This is the R package behind RAND's COVID-19 State policy tool and related papers.
# 
# R Package Author: Pedro Nascimento de Lima
# Model Author: Raffaele Vardavas.
# Copyright (C) 2021 by The RAND Corporation
# See LICENSE.txt and README.txt for information on usage and licensing
#----------------------------------------------------------------------------------------#



#### Compare Models Functions  -------------------------------------------------------####

#----------------------------------------------------------------------------------------#
# Author: Pedro Nascimento de Lima
#
# Purpose: Generic Functions for Comparing Models
# These functions are used to allow easier comparisons between structurally-different models
# The idea behind this function is that when changing the model structure, we should be
# looking at several models simultaneously.
#
# Creation Date: July 2020
#----------------------------------------------------------------------------------------#

#' Set Calibration Experimental Design
#' 
#' This function is similar to the set_experimental_design function, but it assumes
#' you want to calibrate the same model with different parameters. These parameters
#' usually change the model structure.
#'
#' @param model The c19model object
#'
#' @return The model object including a calibration_experimental_design data.frame.
#' @export
#' @importFrom lhs randomLHS
#' @import dplyr
set_model_comparison_experimental_design = function(model, location_ids, inputs_path) {
  
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
  
  model$calibration_comparison_design = grid_params
  
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
evaluate_calibration_experiments = function(model, sim_end_date, inputs_path, location_ids, n_lhs, runs = 1:nrow(model$future_experimental_design), n_cores = parallel::detectCores() - 2, parallel_mode = "PSOCK", solver = "lsoda", write_csv = T, test_run = F) {
  
  ### Filtering Relevant Timeseries:
  data = model$timeseries %>%
    mutate(Source = "Data", fit_rank = 1, RunID = 0, GridExperimentID = 0) %>%
    select(Source, LocationID, fit_rank, PortfolioID, GridExperimentID, RunID, Date, Deaths, CumulativeDeaths, CurrentlyHospitalized, CurrentlyInICU, PositiveTests, CumulativePositiveTests) %>%
    filter(LocationID %in% location_ids) %>%
    as.data.frame()
  
  ## Writing Files before starting the calibration:
  write.table(x = model$calibration_comparison_design, file = "./output/calibration_comparison_design.csv", row.names = F, sep = ",")
  write.table(x = data, file = "./output/results_and_data.csv", row.names = F, sep = ",")
  write.table(x = model$location, file = "./output/location.csv", row.names = F, sep = ",")
  
  ### This is implemented as a For Loop Because the Calibration itself is paralellized:
  for (i in 1:nrow(model$calibration_comparison_design)){
    
    experimental_design = model$calibration_comparison_design[i,]
    
    cat_green_tick(paste0("Starting Calibration Exercise ", i, " of ", nrow(model$calibration_comparison_design)))
    
    # If parameter Inputs File Exists, read it and substitute parameters with new parameters:
    if("InputsFile" %in% colnames(experimental_design)) {
      # Here I'm only reading the parameter table:
      model$parameter = readxl::read_xlsx(path = paste0(inputs_path,"/", experimental_design$InputsFile, ".xlsx"), sheet = "parameter") %>% as.data.frame()
    }
    
    # Add each variable in the Experimental Design to the model list:
    for(variable in names(experimental_design)) {
      
      model[[variable]] = experimental_design[[variable]]
      
    }
    
    ## Now We Can Calibrate the Model:
    calibrated_model = model %>%
      calibrate(model = ., location_ids = location_ids, n_lhs = n_lhs, parallel_mode = parallel_mode, cores = n_cores, test_run = test_run)
      
    calibrated_model = calibrated_model %>%  
      compute_augmented_outputs(model = ., compute_econ_outputs = T, calibration = T)
    
    # Recover All Calibration results And Parameters For the Maximum Date, select 
    detailed_results_and_parameters = calibrated_model$cal_results %>%
      filter(Date %in% c(max(Date))) %>%
      # First Rank Runs by their Fit:
      arrange(LocationID, fit.WeightedRMSE) %>%
      group_by(LocationID) %>%
      mutate(fit_rank = row_number(), GridExperimentID = experimental_design$GridExperimentID) %>%
      left_join(calibrated_model$scenarios, by = "RunID")
    
    
    # Selecting Relevant Results:
    timeseries_augm_results = detailed_results_and_parameters %>%
      ungroup() %>%
      select(RunID, fit_rank) %>%
      left_join(calibrated_model$cali_augm_results, by = "RunID") %>%
      mutate(Source = "Model", GridExperimentID = experimental_design$GridExperimentID) %>%
      select(one_of(names(data)))  %>%
      as.data.frame()
    
    # Get Both Calibration Data and  
    # cal_results$GridExperimentID = experimental_design$GridExperimentID
    # cal_augm_results$GridExperimentID = experimental_design$GridExperimentID
    # parameters$GridExperimentID = experimental_design$GridExperimentID
    
    write.table(x = detailed_results_and_parameters, file = "./output/detailed_results_and_parameters.csv", append = T, row.names = F, sep = ",")
    write.table(x = timeseries_augm_results, file = "./output/results_and_data.csv", append = T, row.names = F, sep = ",")
    #write.table(x = parameters, file = "./output/parameters.csv", append = T, row.names = F, sep = ",")
    
    if(i == 1) {
      final_detailed_results_and_parameters = detailed_results_and_parameters
      final_results_and_data = rbind(data, timeseries_augm_results)
    } else {
      final_detailed_results_and_parameters = rbind(final_detailed_results_and_parameters, detailed_results_and_parameters)
      final_results_and_data = rbind(final_results_and_data, timeseries_augm_results)
    }
    
  }
  
  cat_green_tick(paste0(Sys.time(), " - Finished Comparing Models."))
  
  list(
    model = model,
    detailed_results_and_parameters = final_detailed_results_and_parameters,
    results_and_data = final_results_and_data
    )
  
}



