

#----------------------------------------------------------------------------------------#
# c19randepimod R package
# This is the R package behind RAND's COVID-19 State policy tool and related papers.
# 
# R Package Author: Pedro Nascimento de Lima
# Model Author: Raffaele Vardavas.
# Copyright (C) 2021 by The RAND Corporation
# See LICENSE.txt and README.txt for information on usage and licensing
#----------------------------------------------------------------------------------------#



#### Update and Run Model-------------------------------------------------------------####

#----------------------------------------------------------------------------------------#
# Author: Pedro Nascimento de Lima
# Purpose: This File contains functions to calibrate and run the model for all locations.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#


#' Calibrates and Runs The Model for a set of Locations
#'
#' @param calibrated_model An optional calibrated model object
#' @param location_ids The location ids to run scenarios
#' @param calibrate TRUE if you want to calbrate the model
#' @param n_calibration The number of samples to use in the calibration process
#' @param n_uncertainty The number of samples to use when analysing uncertainties
#' @param portfolio_combination_mode use "new_normal" to revert back to new normal protfolios
#' @param portfolio_ids1 first portfolio to use
#' @param start_dates when to change to each portfolio
#' @param duration1 number of days to stay in first portfolio
#' @param portfolio_ids2 second portfolio to use
#' @param start_date Start Date of simulation.
#' @param sim_end_date End date of simulation aaaa-mm-dd
#' @param augm_inputs_path Path to the augmented inputs object that will be used.
#' @param parallel_mode FORK or PSOCK
#' @param append whether or not to append
#' @param cores number of CPU cores to use.
#'
#' @return The scenario_runs object
#' @export
update_and_calibrate_covid19model = function(calibrated_model, location_ids, calibrate = T, n_calibration, n_uncertainty, portfolio_combination_mode,
                                             portfolio_ids1,
                                             start_dates,
                                             duration1,
                                             portfolio_ids2, start_date, sim_end_date, augm_inputs_path, parallel_mode = "FORK", cores, append = T, test_run = F, solver = "lsoda", ...) {
  
  ### I will save objects in the global environment because I want to be able to recover them in case things go wrong.
  
  cat_green_tick("### Welcome to the COVID-19 Model ###")
  start_updt_time = Sys.time()
  cat_green_tick(start_updt_time)
  
  cat_green_tick(paste0(Sys.time(), " - This script automates the job of updating the COVID-19 model."))
  
  cat_green_tick(paste0(Sys.time(), " - Getting the latest data from the augm_inputs file. Make sure to update this file before running!"))
  
  # Create Inputs Object:
  
  cat_green_tick(paste0(Sys.time(), " - Starting Calibration Process"))
  
  if(calibrate) {
    
    augm_inputs <- readRDS(augm_inputs_path)
    #augm_inputs <<- readRDS("/home/RAND.ORG/rvardava/Projects/covid-19/augm_inputs.rds")
    
    model <- augm_inputs %>%
      prepare_inputs() %>%
      c19model_amshi2s(., level = "state")
    
    print(model)
    
    
    calibrated_model <- model %>%
      calibrate(.,location_ids = location_ids, n_lhs = n_calibration)
    
    print(calibrated_model)
    
    cat_green_tick(paste0(Sys.time(), " - Computing Augmented Calibration Results"))
    
    calibrated_model <- calibrated_model %>%
      compute_augmented_outputs(., compute_econ_outputs = T, calibration = T)
    
  }
  
  
  # calibrated_model = calibrated_model
  #
  cat_green_tick(paste0(Sys.time()," - Running Scenarios for ", length(location_ids), " US states. This will take awhile."))
  
  scenario_runs <- run_all_scenarios(calibrated_model,
                                     location_ids = location_ids,
                                     portfolio_ids1 = portfolio_ids1,
                                     start_dates = start_dates,
                                     duration1 = duration1,
                                     portfolio_ids2 = portfolio_ids2,
                                     portfolio_combination_mode = portfolio_combination_mode,
                                     uncertainty_mode = "best_fitting",
                                     uncertainty_runs = n_uncertainty,
                                     sim_end_date = sim_end_date, parallel_mode = parallel_mode, cores = cores, solver = solver, test_run = test_run, ...)
  
  ### Checking Runs with Integration Errors:
  
  problematic_runs = scenario_runs$scenarios_results %>%
    filter(Deaths < 0 || Hospitalized < 0 || ICUAdmissions < 0 || PositiveTests < 0)
  
  problematic_run_ids <<- unique(problematic_runs$RunID)
  
  if(length(problematic_run_ids)>0) {
    cat_green_tick(paste0("Identified ", length(problematic_run_ids), " runs with integration errors. Check the problematic run ids object."))
    
    scenario_runs$scenarios_results = scenario_runs$scenarios_results %>%
      filter(!(RunID %in% problematic_run_ids))
    
  }
  
  cat_green_tick(paste0(Sys.time()," - Generating Data for the Web Tool ", length(location_ids), " US states. This will take awhile."))
  
  generate_webtool_data(model = calibrated_model, scenario_runs = scenario_runs, update_past_data = F, update_scenarios = T, append = append)
  
  
  #saveRDS(calibrated_model, "calibrated_model.rds")
  
  cat_green_tick(paste0(Sys.time()," - We're done here."))
  
  scenario_runs
  
}