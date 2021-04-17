

#------------------------------------------------------------------------------#
# Code for the paper: Reopening Under Deep Uncertainty: 
#                     Seeking Robust, Non-Dominated COVID-19 Exit Strategies.
# 
# Author: Pedro Nascimento de Lima
# Copyright (C) 2021 by The RAND Corporation
# See LICENSE.txt and README.txt for information on usage and licensing
#------------------------------------------------------------------------------#


# This specialized function abstracts the task of defining the experimental design.
# Originally, these steps were part of a script.
setup_rdm_experiments = function(model_object, model_class_file, n_new_lhs, output_path = "./02_future_runs/outputs/"){
  
  

# Function to compute additional outputs ----------------------------------

  compute_experiment_output = function(results, model) {
    
    # Compute Economic outputs here:
    
    # If results is atomic, it means there are no results:
    if(!is.atomic(results)){
      
      # Economic Outputs:
      results = results %>%
        left_join(model$economicinputs %>%
                    filter(EconSensitivity == "B") %>%
                    select(LocationID, PortfolioID, DailyAbsoluteIncomeLoss, 
                           DailyPercentIncomeLoss), 
                  by = c("LocationID", "PortfolioID"))
      
      # Cumulative Income Loss, Prevalence:
      results = results %>%
        mutate(CumulativeIncomeLoss = cumsum(DailyAbsoluteIncomeLoss),
               CumulativePercentIncomeLoss = cumsum(DailyPercentIncomeLoss),
               Prevalence =  YSs + YSm + YA + CurrentlyInICU + CurrentlyHospitalized,
               SeverePrevalence = YSs + CurrentlyInICU + CurrentlyHospitalized) # %>%
      
      
      # These vectors are used to select specific subpopulations.
      strata_vector = 1:nrow(model$subpopulation)
      frail_pop_vector = 4:5
      
      # Computing Additional Variables 
      vacc_variables = paste0("CumulativeVaccinated", strata_vector)
      frail_vacc_variables = paste0("CumulativeVaccinated", frail_pop_vector)
      
      # Peak Variables, days of interventions:
      results = results %>%
        mutate("Vaccination Coverage" = rowSums(results[,vacc_variables])) %>%
        mutate(
          PeakICU = cummax(CurrentlyInICU),
          PeakHospitalizations = cummax(CurrentlyHospitalized),
          days_of_strict_interventions = cumsum(PortfolioID>4),
          days_of_interventions = cumsum(PortfolioID>1)) %>% 
        ungroup(.)
      
      
      # YLL
      yll_variables = paste0("CumulativeReportedDeaths", strata_vector)
      yll_weights = c(69.2, 20.25, 20.25, 9.52, 20.25)
      
      results$YLL = rowSums(results[,yll_variables] * yll_weights)
      
      
    } else {
      results = NULL
    }
    
    results
  }
  
  
  

# Setting up the Experimental Design --------------------------------------

  experiment_model_rdm = model_object %>%
    # Vaccination Policy:
    set_parameter(model = ., parameter_name = "vaccination_strategy_name",
                  # Options: c("No Vaccine", "Frailty-Based", "Mixing-Based-No-Intervention", "Mixing-Based-Essential-Workers", "Proportional")
                  experimental_design = "grid", values = c("ACIP")) %>%
    # Reopening Policies:
    # Options: c("adaptive-vacc-based", "adaptive-time-based")
    set_parameter(model = ., parameter_name = "npi_type",
                  experimental_design = "grid", values = c("adaptive-time-based", "adaptive-vacc-based")) %>%
    # Baseline Level of Caution:
    set_parameter(model = ., parameter_name = "LevelOfCaution",
                  experimental_design = "grid", values = c(0.5,1.5,3,6,12,24)) %>%
    # Adaptive, Time-Based Parameters:
    set_parameter(model = ., parameter_name = "TransitionDate",
                  experimental_design = "grid", values = lubridate::as_date(c("2021-03-10", "2021-07-04", "2021-09-26"))) %>%
    set_parameter(model = ., parameter_name = "NewLevelOfCautionMultiplier",
                  experimental_design = "grid", values = c(0.1,0.5,1)) %>%
    # Adaptive, Vaccine-Based Parameters:
    #(10,30)
    set_parameter(model = ., parameter_name = "AdaptiveCautionRelaxationRate",
                  experimental_design = "grid", values = c(10, 15)) %>% 
    set_parameter(model = ., parameter_name = "AdaptiveCautionMidpoint",
                  experimental_design = "grid", values = c(0.6,0.4,0.5)) %>%
    # Final Reopening Criteria - It only matters for the time-based policies
    set_parameter(model = ., parameter_name = "reopening_immunity_threshold",
                  experimental_design = "grid", values = c(0.7)) %>%
    # Can be Either All population or Old & Frail:
    set_parameter(model = ., parameter_name = "reopening_criteria",
                  experimental_design = "grid", values = c("All Population")) %>%
    # Vaccination Uncertainties:
    set_parameter(model = ., parameter_name = "mult.trans.efficacy.factor",
                  experimental_design = "lhs",min = 0.1, max = 1) %>%
    set_parameter(model = ., parameter_name = "mult.behavioral.increase.mixing",
                  experimental_design = "lhs",min = 1, max = 1.2) %>%
    set_parameter(model = ., parameter_name = "mult.v.rate",
                  experimental_design = "lhs",min = 0.75, max = 1.25) %>%
    set_parameter(model = ., parameter_name = "mult.V.will.factor",
                  experimental_design = "lhs",min = 0.6, max = 1) %>%
    # Behavioral Uncertainty or New Strains:
    set_parameter(model = ., parameter_name = "PercentChangeInTransmissibility",
                  experimental_design = "lhs",min = -0.25, max = 0.5) %>%
    set_parameter(model = ., parameter_name = "mult.MonthsOfImmunityDuration",
                  experimental_design = "lhs",min = 0.5, max = 2) %>%
    set_experimental_design(model = ., n_new_lhs = n_new_lhs)

  
  # # Make Sure nothing is a factor:
  
  experiment_model_rdm$future_experimental_design <- experiment_model_rdm$future_experimental_design %>%
    dplyr::mutate_if(is.factor, as.character)
  
  experiment_model_rdm$compute_experiment_output = compute_experiment_output
  
  
  # Removing non-sensical strategy combinations:
  experiment_model_rdm$future_experimental_design = experiment_model_rdm$future_experimental_design %>%
    # Remove experiments if paramters do not pertain to that policy:
    filter(!((npi_type == "adaptive-time-based") & ((AdaptiveCautionRelaxationRate != min(AdaptiveCautionRelaxationRate)) | (AdaptiveCautionMidpoint != min(AdaptiveCautionMidpoint)))   )) %>%
    filter(!((npi_type == "adaptive-vacc-based") & ((TransitionDate != min(TransitionDate)) | (NewLevelOfCautionMultiplier != min(NewLevelOfCautionMultiplier)))   )) %>%
    filter(!((npi_type == "adaptive-time-based" & NewLevelOfCautionMultiplier == 1) & (TransitionDate != min(TransitionDate))))
  
  
  sow_experiments = unique(experiment_model_rdm$future_experimental_design %>% select(RunID, LHSExperimentID))
  
  # Randomizing the order of the sow_experiments:
  sow_experiments = sow_experiments[sample(nrow(sow_experiments), replace = F),]
  
  # adding the grid experiment ids:
  
  GridExperimentID = unique(experiment_model_rdm$future_experimental_design$GridExperimentID)
  
  experiments_order = tidyr::expand_grid(sow_experiments, GridExperimentID)
  
  future_experimental_design = experiments_order %>%
    dplyr::left_join(experiment_model_rdm$future_experimental_design, by = c("RunID", "LHSExperimentID", "GridExperimentID")) %>%
    mutate(ExperimentID = row_number())

  experiment_model_rdm$future_experimental_design = future_experimental_design
  
  print(paste0("# of Strategies: ", length(unique(experiment_model_rdm$future_experimental_design$GridExperimentID))))
  
  print(paste0("# of Calibration parameter vectors: ", length(unique(experiment_model_rdm$future_experimental_design$RunID))))
  
  print(paste0("# of Deeply Uncertain parameter vectors: ", length(unique(experiment_model_rdm$future_experimental_design$LHSExperimentID))))
  
  print(paste0("Total # of Runs: ", nrow(experiment_model_rdm$future_experimental_design))) 
  


  saveRDS(experiment_model_rdm, file = paste0(output_path, "experiment_model_rdm.rds"))
  
  # And experimental designs are in the experimental design object
  write.csv(x = experiment_model_rdm$future_experimental_design, 
            file = paste0(output_path, "experimental_design.csv"), row.names = F)
  
  
  # OTher tables, like the location and the calibration parameters also can be
  # useful
  write.csv(x = experiment_model_rdm$location, 
            file = paste0(output_path, "location.csv"), row.names = F)
  
  # Scenarios are actually parameters used in the calibration phase.
  write.csv(x = experiment_model_rdm$scenarios, 
            file = paste0(output_path, "scenarios.csv"), row.names = F)
  
  
  # Evaluate Experiments:
  source(model_class_file)
  experiment_model_rdm$model_fn = model_function.c19model_vamshi4s
  experiment_model_rdm$set_calibrated_stocks = set_calibrated_stocks.c19model_vamshi4s
  experiment_model_rdm$set_computed_params = set_computed_params.c19model_vamshi4s
  experiment_model_rdm$pre_compute = pre_compute_function.c19model_vamshi4s
  
  
  experiment_model_rdm
  
}
