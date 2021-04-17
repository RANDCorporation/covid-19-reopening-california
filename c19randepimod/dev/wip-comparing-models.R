
library(dplyr)
# Create Sample Model File
augm_inputs = get_augmented_inputs(inputs_path = "../analyses/rand-webtool/inputs", write_to_server = F, read_from_web = T, start_date = "2020-03-01", calibration_date = "2020-04-07")

# Create a Calibrated Model:
basemodel <- augm_inputs %>%
  c19model_amshi2s(., level = "state")

basemodel = basemodel %>%
  set_parameter(model = ., parameter_name = "ModelVersion", experimental_design = "grid", values = c("Standard")) %>%
  set_parameter(model = ., parameter_name = "InputsFile", experimental_design = "grid", values = c("input_dataset", "expanded_params")) %>%
  set_model_comparison_experimental_design(model = .)

model_result = evaluate_calibration_experiments(model = basemodel, n_lhs = 5, location_ids = c(5,35), inputs_path = "../analyses/rand-webtool/inputs", sim_end_date = Sys.Date()-10,n_cores = 5, parallel_mode = "PSOCK")
