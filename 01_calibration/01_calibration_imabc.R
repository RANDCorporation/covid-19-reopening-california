

#------------------------------------------------------------------------------#
# Code for the paper: Reopening Under Deep Uncertainty: 
#                     Seeking Robust, Non-Dominated COVID-19 Exit Strategies.
# 
# Author: Pedro Nascimento de Lima
# Copyright (C) 2021 by The RAND Corporation
# See LICENSE.txt and README.txt for information on usage and licensing
#------------------------------------------------------------------------------#

#### MODEL CALIBRATION SCRIPT  ---------------------------------------------####

#------------------------------------------------------------------------------#
# Author: Pedro Nascimento de Lima
# Purpose: This script calibrates the model in parallel using the IMABC approach.
# 
# These functions are specialized for the COVID-19 RDM Analysis paper.
#------------------------------------------------------------------------------#


#### Step 0 - Sourcing the Model, packages, settings------------------------####

# Sourcing relevant files.
source("./00_dependencies/load_libraries.R")
source("model_class.R")
source("01_calibration/calibrate_imabc.R")


# Global Options
N_CORES = 35 # change to the number of cores you want to use
PARALLEL_MODE = "FORK" # use"PSOCK" if using windows
LOCATION_IDS = 5 # State Id to use. 5 = California


#### Step 1 - Getting Model Inputs  ----------------------------------------####
augm_inputs = get_augmented_inputs(inputs_path = "./01_calibration/inputs/",
                                   inputs_filename = "input_dataset_ew_2021-02-params-less-constrained-bounds-level-06-nov-21-lower-seasonality.xlsx", 
                                   write_to_server = F, read_from_web = T, 
                                   start_date = "2020-03-01", 
                                   calibration_date = "2020-04-07")
saveRDS(augm_inputs, file = "01_calibration/inputs/augm_inputs.rds")

# Filtering the dates so we get exactly 10 targets
augm_inputs$locationtimeseries = augm_inputs$locationtimeseries %>%
  filter(Date <= min(Date) + (30*10)-1)

augm_inputs$max_ts_date = max(augm_inputs$locationtimeseries$Date)

# Create model object from the desired model class.
model <- augm_inputs %>%
  c19model_vamshi4s(., level = "state")

model$model_fn = model_function.c19model_vamshi4s
model$timetolerance = 10 * 60

# Adding a minimum number of susceptibles to be used as a lower bound target.
model$timeseries = model$timeseries %>%
  mutate(Susceptible = 0.7 * Population)

Sys.time()

source("01_calibration/calibrate_imabc.R")

# Run for the second time, now using existing results:
calibrated_model_imabc = calibrate_imabc(
  #previous_results = calibrated_model_imabc,
  model=model, 
  location_ids=5, 
  minR0 = 2.6, 
  maxR0 = 4, 
  dt=1/4,  
  model_outcomes = c("CumulativeDeaths", "Susceptible"),
  data_outcomes = c("CumulativeDeaths", "Susceptible"),
  weights_list =  list(c(0.3, 0.3, 0.2, 0.2), c(0.05, 0.42, 0.01, 0.42)),
  # zero_bounds = c(-1e-3, 20000, -1e-3, 10000),
  roll_avg = 30, 
  N_start = 3500, # 1000
  seed = 1234,
  latinHypercube = TRUE,
  N_centers = 20,
  Center_n = 25,
  N_post = 100,
  max_iter = 10, # 10
  N_cov_points = 100,
  sample_inflate = 10,   #inflate more so we can more draws at get_B_draws
  verbose = TRUE,
  output_directory = "01_calibration/outputs/imabc",
  output_tag = "cali_long",
  #iter_parm_draws = iter_parm_draws,
  #parms_to_run = parms_to_run,
  #iter_sim_results = iter_sim_results,
  #previous_results_dir = "01_calibration/outputs/N_start_350",
  #previous_results_tag = "cali_long",
  cores = N_CORES)


library(c19randepimod)

imabc_calibrated_model = model

scenarios = readRDS("./01_calibration/outputs/param.df.good.rds") %>%
  select(-seed,-step,-draw, -scaled_dist, -RunID)

imabc_calibrated_model$scenarios = scenarios

Sys.time()

# Sample Parameters using IMABC´s weights:

set.seed(1234)

# Sampling with IMABC´s weights:
scenario_ids_sample = sample(scenarios$ScenarioID,size = 100, replace = T, prob = scenarios$sample_wt) 

unique_scenario_ids = unique(scenario_ids_sample)
length(unique_scenario_ids)

selected_scenarios = scenarios %>%
  filter(ScenarioID %in% scenario_ids_sample)

# Filtering only the parameter sets chosen: 
imabc_calibrated_model$scenarios = selected_scenarios

# scenarios:
imabc_calibrated_model$scenario_run_ids_sample = scenario_ids_sample


imabc_calibrated_model = imabc_calibrated_model %>%
  c19randepimod::calibrate(model = ., location_ids = 5, n_lhs = 1, parallel_mode = "FORK", cores = 35, scenarios = selected_scenarios)

imabc_calibrated_model = imabc_calibrated_model %>%
  c19randepimod::compute_augmented_outputs(model = ., compute_econ_outputs = T, calibration = T)


saveRDS(imabc_calibrated_model, file = "./01_calibration/outputs/calibrated_model.rds")


imabc_calibrated_model = readRDS("./01_calibration/outputs/calibrated_model.rds")


source("./01_calibration/02_calibration_figures.R")

calibration_plot30 = calibration_plot_week(imabc_calibrated_model, range = 0.2, max_range_factor = 2.5, s_range = c(0.6,1), filter_in_range = T, time_window_width = 30)
calibration_plot7 = calibration_plot_week(imabc_calibrated_model, range = 0.2,max_range_factor = 2.5, s_range = c(0.6,1), filter_in_range = T, time_window_width = 7)

calibration_plot30$plot

calibration_plot7$plot

ggsave(filename = paste0("./01_calibration/outputs/calibration_plot30_imabc.pdf"), plot = calibration_plot30$plot, device = "pdf", width = 7, height = 2, units = "in", scale = 2)
ggsave(filename = paste0("./01_calibration/outputs/calibration_plot7_imabc.pdf"), plot = calibration_plot7$plot, device = "pdf", width = 7, height = 2, units = "in", scale = 2)

print(paste0("Saving calibrated model with ", length(unique(imabc_calibrated_model$cali_augm_results$RunID)), " unique runs."))












# Seasonality Shift



source("model_class.R")

imabc_calibrated_model$pre_compute = pre_compute_function.c19model_vamshi4s

imabc_calibrated_model$scenarios$seas_shift = -20

selected_scenarios = imabc_calibrated_model$scenarios

imabc_calibrated_model_shift = imabc_calibrated_model %>%
  c19randepimod::calibrate(model = ., location_ids = 5, n_lhs = 1, parallel_mode = "FORK", cores = 35, scenarios = selected_scenarios)

imabc_calibrated_model_shift = imabc_calibrated_model_shift %>%
  c19randepimod::compute_augmented_outputs(model = ., compute_econ_outputs = T, calibration = T)


source("./01_calibration/02_calibration_figures.R")

calibration_plot30 = calibration_plot_week(imabc_calibrated_model, range = 0.2, max_range_factor = 2.5, s_range = c(0.6,1), filter_in_range = F, time_window_width = 30)
calibration_plot7 = calibration_plot_week(imabc_calibrated_model, range = 0.2,max_range_factor = 2.5, s_range = c(0.6,1), filter_in_range = F, time_window_width = 7)

calibration_plot30$plot

calibration_plot7$plot







