

#----------------------------------------------------------------------------------------#
# c19randepimod R package
# This is the R package behind RAND's COVID-19 State policy tool and related papers.
# 
# R Package Author: Pedro Nascimento de Lima
# Model Author: Raffaele Vardavas.
# Copyright (C) 2021 by The RAND Corporation
# See LICENSE.txt and README.txt for information on usage and licensing
#----------------------------------------------------------------------------------------#



#### Webtool Functions ---------------------------------------------------------------####

#----------------------------------------------------------------------------------------#
# Author: Pedro Nascimento de Lima
# Purpose: This File contains functions that generate data for RAND's COVID-19 Webtool
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#


#' Update Daily Timeseries Data
#'
#' Updates csv files that are used by the web tool api, runs all tests and save the test logs.
#'
#' @param datafolder path in which to save the csv files
#' @param inputs path to inputs_dataset spreadsheet
#' @export
#' 
#' @importFrom testthat test_dir
#'
update_daily_timeseries = function(inputs = "./data/inputs", datafolder = "../../covid19-cloudsearch-tool/data_files/") {
  
  cat_green_tick("Getting Inputs Object")
  
  # Create Inputs Object:
  augm_inputs = get_augmented_inputs(inputs_path = inputs, start_date = "2020-03-01", write_to_server = F)
  
  saveRDS(augm_inputs, "augm_inputs.rds")
  
  #saveRDS(augm_inputs, "augm_inputs.rds")
  model = augm_inputs %>%
    prepare_inputs() %>%
    c19model_amshi2s(., level = "state")
  
  cat_green_tick("Generating Webtool Data")
  
  generate_webtool_data(model, update_past_data = T, update_scenarios = F, folder = datafolder)
  
  
  cat_green_tick("Testing File Consistency")
  
  outputs_tests_dir = system.file("outputs_tests", package = "c19randepimod")
  
  sink(paste0(datafolder, 'tests_log.txt'), split = T)
  
  test_dir(outputs_tests_dir)
  
  sink()
  
  cat_green_tick("Done.")
  
}