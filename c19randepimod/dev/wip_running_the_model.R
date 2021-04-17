
### How we Should be Able to Run the Model ####

library(c19randepimod)
library(dplyr)
## Calibrating the Model:
augm_inputs = get_augmented_inputs(inputs_path = "../analyses/rdm-analysis/inputs",inputs_filename = "input_dataset.xlsx", write_to_server = F, read_from_web = T, start_date = "2020-03-01", calibration_date = "2020-04-07")

calibrated_model <- augm_inputs %>%
  c19model_amshi2s(., level = "state") %>%
  calibrate(.,location_ids = c(5, 35), n_lhs = 14, cores = 7, parallel_mode = "PSOCK")

calibrated_model = calibrated_model %>%
  compute_augmented_outputs(calibration = T)


### Setting Up the Experimental Design Variables:

experiment_model = calibrated_model %>%
  get_best_fit_model(model = ., n = 2) %>%
  set_parameter(model = ., parameter_name = "SomeParameter", experimental_design = "lhs", min = 0.1, max = 0.2) %>%
  set_parameter(model = ., parameter_name = "Parameter2", experimental_design = "lhs", min = 0.1, max = 0.2) %>%
  set_parameter(model = ., parameter_name = "Parameter3", experimental_design = "lhs", min = 0.1, max = 0.2) %>%
  set_parameter(model = ., parameter_name = "Parameter4", experimental_design = "lhs", min = 0.1, max = 0.2) %>%
  set_parameter(model = ., parameter_name = "parameter5", experimental_design = "lhs", min = 0.1, max = 0.2) %>%
  set_parameter(model = ., parameter_name = "parameter6", experimental_design = "lhs", min = 0.1, max = 0.2) %>%
  set_parameter(model = ., parameter_name = "Parameter7", experimental_design = "lhs", min = 0.1, max = 0.2) %>%
  set_parameter(model = ., parameter_name = "VacciationPolicy", experimental_design = "grid", values = c("Young Poeple", "Frail People")) %>%
  set_parameter(model = ., parameter_name = "AnotherParameter", experimental_design = "grid", values = c(3,2,3,4)) %>%
  set_experimental_design(model = ., n_new_lhs = 3)

# Each Experiment can have a function that is used to post-process the simulation outcomes. This Function is executed in parallel and thus should not depend on external libraries other than the c19randepimod package and dplyr.
experiment_model$compute_experiment_output = function(results) {
  results %>%
    dplyr::select(time, CumulativeDeaths, PortfolioID, LocationID, ExperimentID, RunID)
}

nrow(experiment_model$future_experimental_design)

results = run_experiment(experiment_id = 1, model = experiment_model, sim_end_date = "2020-09-01")

results = evaluate_experiment(model = experiment_model, runs = 1:20, sim_end_date = "2020-09-01")


  
View(experiment_model$future_experimental_design)









  

experiment_model$future_experimental_design

experiment_results = experiment_model %>%
  evaluate_experiment(model = ., n_cores = 1, n_new_lhs = 10)


