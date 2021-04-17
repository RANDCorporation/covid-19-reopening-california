

#----------------------------------------------------------------------------------------#
# c19randepimod R package
# This is the R package behind RAND's COVID-19 State policy tool and related papers.
# 
# Author: Pedro Nascimento de Lima
# Model Author: Raffaele Vardavas.
# Copyright (C) 2021 by The RAND Corporation
# See LICENSE.txt and README.txt for information on usage and licensing
#----------------------------------------------------------------------------------------#


#### c19model S3 Class ---------------------------------------------------------------####

#----------------------------------------------------------------------------------------#
# Author: Pedro Nascimento de Lima
# Purpose: This File contains generic functions that are used across all c19models.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#


#' c19model Class constructor / factory / generator
#' 
#' This is a template class constructor for models based on the c19model class.
#'
#' @param inputs the inputs object created with get_augmented_inputs
#' @param level either "state" or "country".
#'
#' @return a c19model object.
#' @export
#'
c19model = function(inputs, level) {
  
  model = inputs %>%
    uniformize_inputs_level(.,level = level)
  # Initiate Covid-19 model as an empty class.
  # Defining the model geographic level:
  
  model$level = level
  
  # Uniformize Inputs according to the intended level:
  class(inputs) = "c19model"
  
  inputs
}



#----------------------------------------------------------------------------------------#
# Function: Generic Functions
# Purpose: These generic functions can be defined for each model that is built of the
# c19model class.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#

set_initial_stocks = function(model, initial_exposed) {
  UseMethod("set_initial_stocks", model)
}

set_calibrated_stocks = function(model, ...) {
  UseMethod("set_calibrated_stocks", model)
}

set_computed_params = function(model, ...){
  UseMethod(generic = "set_computed_params",model)
}

model_function = function(model, ...) {
  UseMethod("model_function", model)
}

verify_input_parameters = function(model, ...) {
  UseMethod("verify_input_parameters", model)
}

set_stocks_positions = function(model, ...){
  UseMethod("set_stocks_positions", model)
}

set_capacities = function(model, ...){
  UseMethod("set_capacities", model)
}

# By default, set stocks positions does nothing. Nevertheless this function is used in models that have strata.
set_stocks_positions.c19model = function(model){
  model
}

set_capacities.c19model = function(model){
  model
}

#### Calibration - Location-Dependent Functions ####

#----------------------------------------------------------------------------------------#
# Calibration Functions
# Purpose: These functions are used to calibrate the model.
# c19model class.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#

#' Set Calibrated K Factor
#'
#' The models built out based on Raffaelle Vardava's AMSHI model need a proportionality factor k to run. This function computes this factor for a set of locations, while creating a latin hypercube sample of unknown parameters.
#'
#' @param model a model object of the c19model class
#' @param location_ids vector of locations for which to compute the k factor
#' @param n_lhs integer defining the number of samples to create
#' @param minR0 Minimum R0 that will be used for states without enough data
#' @param maxR0 Maximum R0 that will be used for locations without enough data
#'
#' @return a model object of the c19model class
set_calibrated_k = function(model, location_ids, n_lhs = 2, minR0 = 2, maxR0 = 4, scenarios=NULL) {
  
  
  # If a scenarios object is not passed to the function, create one:
  if(is.null(scenarios)){
    model = model %>%
      set_lhs_scenarios(.,n_lhs = n_lhs, lhs_mode = "calibration", probabilistic = T)  
  } else {
    model$scenarios = scenarios
  }
  
  # Run the get param function, compute tau for each scenario.
  model = model %>%
    model$set_computed_params(.)
  
  runs_locations = as.data.frame(expand.grid(model$scenarios[,"ScenarioID"], location_ids))
  
  names(runs_locations) = c("ScenarioID", "LocationID")
  
  
  # Joining the data we need:
  model$scenarios = runs_locations %>%
    left_join(as.data.frame(model$scenarios), by = "ScenarioID") %>%
    left_join(model$location, by = "LocationID")
  
  model$scenarios$RunID = 1:nrow(model$scenarios)
  
  # Before doing any calculations, assert that there are no NAs in Any of the tables.
  
  # Defining the minimum and maximum population density
  minPopDensity = min(model$location$PopulationDensity)
  maxPopDensity = max(model$location$PopulationDensity)
  
  # Computing R0 using either a regression or the baseline R0.
  # The gradient found in the regression is used to the extent to which it fits the data.
  
  model$scenarios = model$scenarios %>%
    mutate(
      # Reference R0 is estimated based on an linear interpolation using the Population Density as a proxy:
      referenceR0 = minR0 + (maxR0 - minR0) * (PopulationDensity - minPopDensity) / (maxPopDensity - minPopDensity),
      # R0 regression means the R0 computed in the run_exponential_regressions function. R0 Regression comes directly from each location data and could be subject to reporting bias. For this reason, we use an weighted average to compute the actual R0.
      R0_regression = T0Coeff * tau + 1,
      # This is a weighted average between the reference R0 and the R0 found in the regression.
      # Original R0 function dampened the curve:
      # R0 =  (1-MaxT0CoeffRSquared) * referenceR0 + MaxT0CoeffRSquared * R0_regression,
      R0 = ifelse(MaxT0CoeffRSquared > 0.8, R0_regression, referenceR0),
      #R1 = T1Coeff * tau + 1,
      cbeta = R0 / tau.eff
      #cbeta1 = R1 / tau.eff
    )
  
  # For now, doing this only for the No Intervention Matrix:
  # For Age Strata models, let's compute cbeta k:
  
  compute_cbeta_pop = function(location_id, model) {
    
    pop_distribution = model$compartment$PopulationShareInLocation[model$compartment$LocationID == location_id]
    
    cbeta_pop = t(pop_distribution) %*% model$cbetamatrices[[1]] %*% pop_distribution
    
    cbeta_pop
  }
  
  cbeta_pop_location = data.frame(LocationID = location_ids,
                                  cbeta_pop = sapply(location_ids, compute_cbeta_pop, model = model))
  
  # Review the K computation:
  model$scenarios = model$scenarios %>%
    left_join(cbeta_pop_location, by = "LocationID") %>%
    mutate(k = R0/(tau.eff*cbeta_pop))
  
  check_model_nas(model)
  
  model
  
}


#' Filter Inputs for simulation
#'
#' Filters the location, economicinputs, timeseries and compartment objects of the selected location_ids
#'
#' @param inputs an inputs list
#' @param location_ids the location ids to filter
#' @param level the model level to use. Currently this is not used.
#'
#' @return the same list
filter_inputs = function(inputs, location_ids, level = "state") {
  
  inputs$location = inputs$location %>% filter(LocationID %in% location_ids)
  
  inputs$economicinputs = inputs$economicinputs %>% filter(LocationID %in% location_ids)
  
  inputs$timeseries = inputs$timeseries %>% filter(LocationID %in% location_ids)
  
  inputs$compartment = inputs$compartment %>% filter(LocationID %in% location_ids)
  
  inputs
  
}


#' Uniformize inputs
#'
#' The inputs spreadsheet contains inputs on the country and state level. This function unformizes the input object so all dataframes and objects have a consistent name.
#'
#' @param inputs an inputs list
#' @param level the level to use (either state, city or country)
#'
#' @return the inputs list
#' @export
uniformize_inputs_level = function(inputs, level) {
  
  if (level %in% c("state", "city")) {
    
    unif_inputs = inputs
    
    unif_inputs$timeseries = inputs$locationtimeseries
    
    unif_inputs$compartment = inputs$locationcompartment
    
    unif_inputs$calibration = inputs$locationcalibration
    
  } else {
    
    ### For Countries, we need to rename some of the variables to make them match the state-level:
    inputs$country = inputs$country %>%
      rename(LocationID = CountryID)
    
    inputs$countrytimeseries = inputs$countrytimeseries %>%
      rename(LocationID = CountryID)
    
    inputs$countrycalibration = inputs$countrycalibration %>%
      rename(LocationID = CountryID)
    
    unif_inputs = inputs
    
    unif_inputs$timeseries = unif_inputs$countrytimeseries
    
    unif_inputs$compartment = unif_inputs$countrycompartment
    
    unif_inputs$location = inputs$country
    
    unif_inputs$calibration = inputs$countrycalibration
    
  }
  
  # Replacing NaNs with zeros:
  unif_inputs$timeseries = unif_inputs$timeseries %>%
    mutate_at(., vars(-group_cols(), -Date), ~replace(., is.nan(.), 0))
  
  # Removing duplicated objects - So the Uniformized Inputs are truly uniform:
  unif_inputs$locationtimeseries = NULL
  unif_inputs$locationcompartment = NULL
  unif_inputs$locationcalibration = NULL
  unif_inputs$countrytimeseries = NULL
  unif_inputs$countrycompartment = NULL
  unif_inputs$country = NULL
  unif_inputs$countrycalibration = NULL
  
  unif_inputs
}


## Add Calibration scenarios to Inputs List:
#' Sets the latin hypercube sample
#'
#' @param model A model of the c19model class
#' @param n_lhs Number of samples
#' @param seed Seed to set, if any
#' @param lhs_mode Should be "calibration" for now, but we may want to define different parameter bounds for different tasks.
#' @param probabilistic If TRUE, uses the probability distribution specified. Uses an uniform distribution otherwise
#'
#' @return The model object including a scenarios object.
#' @importFrom stats qunif
set_lhs_scenarios = function(model, n_lhs = 100, seed = 1, lhs_mode = "calibration", probabilistic = T) {
  
  if(!missing(seed)){
    set.seed(seed)
  }
  
  model_parameters = model$parameter %>% filter(Model == model$model_name)
  
  nvar = nrow(model_parameters)
  
  # Obtaining a hypercube
  
  randomLHS <- lhs::randomLHS(n_lhs, nvar)
  
  p = as.data.frame(randomLHS)
  
  
  # Getting the Reference Value:
  ref = as.vector(model_parameters$ReferenceValue)
  
  if(lhs_mode == "calibration") {
    min = as.vector(model_parameters$MinCalibrationValue)
    max = as.vector(model_parameters$MaxCalibrationValue)
  } else {
    min = as.vector(model_parameters$MinExplorationValue)
    max = as.vector(model_parameters$MaxExplorationValue)
  }
  
  # All Maximums are equal or greater than the minimum
  
  check_that(min(max >= min) == 1, msg = "Verify Your params Tab in the inputs file. All maximums should be at least greater than the minimums.")
  
  param_names = as.vector(model_parameters$InternalName)
  
  dist = as.vector(model_parameters$PDF)
  
  # Transforming the Hypercube
  scenarios = matrix(nrow = n_lhs, ncol = nvar+1)
  
  # Getting the scenarios:
  for (var in param_names) {
    
    i = which(x = param_names == var)
    
    # If we are using only one sample, let's use the reference value:
    if(n_lhs == 1) {
      scenarios[,i+1] = ref[i]
    } else {
      
      # We are only using the PERT distribution and the Uniform:
      if(probabilistic & dist[i] == "PERT" & min[i] < max[i]) {
        scenarios[,i+1] =  qpert(p = randomLHS[,i], x.min = min[i], x.mode = ref[i], x.max = max[i])
      } else {
        # If we're not using a probabilistic aproach, we will just sample from an uniform (after doing the LHS):
        scenarios[,i+1] = qunif(p = randomLHS[,i], min = min[i], max = max[i])
      }
      
    }
    
  }
  
  # Adding Names:
  param_names = c("ScenarioID",param_names)
  
  colnames(scenarios) = param_names
  
  scenarios[,"ScenarioID"] = 1:nrow(scenarios)
  
  model$scenarios = scenarios
  
  model
  
}

# Add Selected Params for Run ID:
set_selected_params_for_run_id = function(model, run_id) {
  model$params = model$scenarios[run_id,]
  model
}

# Add Time Vector:
set_time = function(model, final_time) {
  model$time =  seq.default(from = 1, to = final_time, by = 1)
  model
}

# Setting testing capacity is defined at the model level because it is particular to this model. 
# This could change in the future if testing capacity is widely used in other models.
#' @export
set_capacities = function(model, max_c_testing_factor = 1, min_c_testing_factor = 0.05, max_c_beds_factor = 1, min_c_beds_factor = 0.9, ventilators_to_beds = 1/5, days_until_max_capacity = -30) {
  
  
  capacity_information = model$location %>%
    select(LocationID, TotalHospitalBeds, AvailableHospitalBeds, TotalICUBeds, PotentiallyAvailableHospitalBeds, PotentiallyAvailableICUBeds) %>%
    
    # For every day, compute the positive tests moving average
    
    left_join(model$timeseries %>% select(LocationID, Date, Population, PositiveTestsMovingAverage, CurrentlyHospitalized), by = "LocationID") %>%
    mutate(PositiveTestCapacity = PositiveTestsMovingAverage / Population,
           MaxBedsUsed = pmax(CurrentlyHospitalized, PotentiallyAvailableHospitalBeds)) %>%
    group_by(LocationID) %>%
    
    # Compute the maximum testing capacity.
    summarise(MaxTestingCapacity = max(PositiveTestCapacity) * max_c_testing_factor,
              # This line is important and assures that we keep up surge capacity that is evidently added.
              MaxBedsCapacity = max(MaxBedsUsed / Population) * max_c_beds_factor,
              MinBedsCapacity = MaxBedsCapacity * min_c_beds_factor,  # mean(AvailableHospitalBeds / Population),
              Days = n()) %>%
    mutate(MaxVentilatorCapacity = MaxBedsCapacity * ventilators_to_beds,
           MinVentilatorCapacity = MinBedsCapacity * ventilators_to_beds,
           # The Capacities growth rate assume that maximum capacity will be achieved today.
           VentilatorCapacityGrowthRate = (MaxVentilatorCapacity - MinVentilatorCapacity) / (Days + days_until_max_capacity),
           BedsCapacityGrowthRate = (MaxBedsCapacity - MinBedsCapacity) / (Days + days_until_max_capacity),
           MinTestingCapacity = MaxTestingCapacity * min_c_testing_factor,
           TestingCapacityGrowthRate = (MaxTestingCapacity - MinTestingCapacity) / (Days + days_until_max_capacity)
    )
  
  model$capacities = capacity_information
  
  model
}



### Generic Functions

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

summary.c19model = function(model) {
  model
}
