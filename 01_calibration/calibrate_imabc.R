
#------------------------------------------------------------------------------#
# Code for the paper: Reopening Under Deep Uncertainty: 
#                     Seeking Robust, Non-Dominated COVID-19 Exit Strategies.
# Copyright (C) 2021 by The RAND Corporation
# See LICENSE.txt and README.txt for information on usage and licensing
#------------------------------------------------------------------------------#

#### CALIBRATE MODEL WITH IMABC --------------------------------------------####

#------------------------------------------------------------------------------#
# Original Version Author: Tyna Feloundou
# Author of revised version: Pedro Nascimento de Lima
# Purpose: This function is used to calibrate the model with IMABC
# 
# These functions are specialized for the COVID-19 RDM Analysis paper.
#------------------------------------------------------------------------------#

calibrate_imabc = function(
  previous_results = NULL,
  model, 
  location_ids, 
  minR0 = 2, 
  maxR0 = 4, 
  dt=1/4, 
  model_outcomes,
  data_outcomes,
  weights_list,
  roll_avg = 30, 
  N_start = 1000,
  seed = 12345,
  latinHypercube = TRUE,
  N_centers = 1,
  Center_n = 50,
  N_post = 100,
  max_iter = 1000,
  N_cov_points = 0,
  sample_inflate = 1,
  #recalc_centers = FALSE,
  output_directory = "imabc_calibration_output/",
  output_tag = "covid_weekly",
  previous_results_dir = NULL,
  previous_results_tag = NULL,
  verbose = TRUE,
  #iter_parm_draws = NULL,
  #parms_to_run = NULL,
  #iter_sim_results = NULL,
  cores){

# Pass these options to the global environment, 
# where the cluster export will fetch objects for registration
assign("location_ids", location_ids, env= .GlobalEnv)
assign("minR0", minR0, env= .GlobalEnv)
assign("maxR0", maxR0, env= .GlobalEnv)


# Register clusters
cl <- makeCluster(cores)
clusterExport(cl=cl, c("model", "set_time", "filter_inputs", "set_calibration_dates", 
                       "calibrate_initiation_num", "run_exponential_regressions", "set_selected_params_for_run_id", 
                       "solve_model", "safe_division", "cat_green_tick","cat_red_bullet", "location_ids", "maxR0", "minR0"))

registerDoParallel(cl)



# Priors ------------------------------------------------------------------


# Getting the Reference Value and bounds:
model_parameters = model$parameter %>% filter(Model == model$model_name)

# remove constant priors
model_parameters_priors <- model_parameters %>% 
                                dplyr::filter(MinCalibrationValue != MaxCalibrationValue)

model_parameters_constant <- model_parameters %>% 
                                dplyr::filter(MinCalibrationValue == MaxCalibrationValue) %>% 
                                dplyr::select(InternalName, ReferenceValue)

ref = model_parameters_priors$ReferenceValue
min = model_parameters_priors$MinCalibrationValue
max = model_parameters_priors$MaxCalibrationValue

model_parameters_priors = model_parameters_priors %>%
  rename(parameter_name = InternalName,
         dist_base_name = PDF,
         mean = ReferenceValue,
         min = MinCalibrationValue,
         max = MaxCalibrationValue) %>%
  mutate(a = min,
         b = max,
         sd = NA,
         dist_base_name = "unif") %>%
  dplyr::select(parameter_name, dist_base_name, mean, sd, min, max, a, b)

covid.priors  = as.priors(df = model_parameters_priors)

# Alternative way to set priors using the Pert Distribution:
# prior_list_covid = list()
# for(i in 1:length(ref)){
#   if(model_parameters_priors$PDF[i] == "PERT"){
#     prior_list_covid[[i]] = add_prior(
#     parameter_name = model_parameters_priors$InternalName[i],
#     quantile_fn = "qpert",
#     min=0,
#     max=15,
#     x.min = min[i], 
#     x.mode = ref[i], 
#     x.max = max[i]
#     )
#   } else {
#     prior_list_covid[[i]] = add_prior(
#     parameter_name = model_parameters_priors$InternalName[i],
#     quantile_fn = "qunif",
#     min = min[i], 
#     max = max[i]
#     )
#   }  
# }
#names(prior_list_covid) = model_parameters_priors$InternalName 
# covid.priors <- define_priors(prior_list_covid)




# Calibration Targets -----------------------------------------------------

target_inputs = model %>% filter_inputs(., location_ids = location_ids, level = model$level)

# Filter date
target_inputs$timeseries = target_inputs$timeseries %>%  filter(Date <= max(target_inputs$timeseries$Date))

target_inputs$timeseries = target_inputs$timeseries[!duplicated(target_inputs$timeseries$Date),]


# Collapsing Targets:
for(i in 1:length(model_outcomes)){  # set the multipliers for each target.
  vec <- colMeans(matrix(target_inputs$timeseries[[model_outcomes[i]]], nrow=roll_avg)) %>% as.data.frame() %>% rename(metric = '.')

  target.sub.df  <- target.as.df(ix=i, df = vec, lb_start_mult= weights_list[[i]][1], 
                                                 ub_start_mult= weights_list[[i]][2], 
                                                 lb_stop_mult = weights_list[[i]][3], 
                                                 ub_stop_mult = weights_list[[i]][4])
  
  assign(paste0("target.sub.df", i), target.sub.df)
}

# get list of grouped targets
target_list <- mget(ls(pattern = "target.sub.df\\d+"))

# bind dataframes
target.df <- do.call(rbind, target_list) %>%
  rename(target_names = name,
         targets = target,
         current_lower_bounds = lower_bounds_start,
         current_upper_bounds = upper_bounds_start,
         stopping_lower_bounds = lower_bounds_stop,
         stopping_upper_bounds = upper_bounds_stop
         ) %>%
  mutate(target_names = paste0(group, target_names))

# Defining Targets Object:
covid.targets.rolled <- as.targets(target.df)



# IMABC call --------------------------------------------------------------

covid.results.parallel <- imabc(
  #previous_results = previous_results,
  target_fun = covid.target.fun,
  priors = covid.priors,
  targets = covid.targets.rolled,
  output_directory = output_directory,
  output_tag = output_tag,
  N_start = N_start,
  N_centers = N_centers,
  Center_n = Center_n,
  N_post = N_post,
  seed = seed,
  latinHypercube = latinHypercube,
  max_iter = max_iter,
  N_cov_points = N_cov_points,
  sample_inflate = sample_inflate,
  #recalc_centers = recalc_centers,
  backend_fun = backend_fun_covid,
  verbose = verbose
  #iter_parm_draws = iter_parm_draws,
  #parms_to_run = parms_to_run,
  #iter_sim_results = iter_sim_results
)

# Saving Parameters in a useful form --------------------------------------
good.sim.params <- covid.results.parallel$good_parm_draws

# get constant parameters
model_parameters_constant <- model$parameter %>% filter(Model == model$model_name) %>% 
                                                 filter(MinCalibrationValue == MaxCalibrationValue) %>% 
                                                 select(InternalName, ReferenceValue)


df1 <- as.data.frame(t(model_parameters_constant))
names(df1) <- as.character(unlist(df1["InternalName",]))
df1 <- df1[2,]  

df.full <- lapply(df1, function(x) as.numeric(as.character(x))) %>% as.data.frame()

# defining full parameters table.
param.df.good <- cbind(good.sim.params, df.full) %>% 
  # RunID and Scenario ID are the same because this calibration is running separately for each state.
  mutate(RunID = dplyr::row_number(), 
         ScenarioID = dplyr::row_number())

# Saving objects:
saveRDS(param.df.good, file = "./01_calibration/outputs/param.df.good.rds")
saveRDS(covid.results.parallel, file = "./01_calibration/outputs/imabc_object_result.rds")

return(covid.results.parallel)

}


# Functions specialized for the calibrate_imabc function ------------------


# Creates Targets as Dataframe --------------------------------------------
# Translates a timeseries object to a target data.frame to be used by IMABC

target.as.df <- function(ix, df=target_inputs$timeseries, lb_start_mult, ub_start_mult, lb_stop_mult, ub_stop_mult){
  
  # create targets dataframe
  ts <- df
  colnames(ts) <- "target"
  ts$group <- paste0("G", ix)
  ts$name <- paste0("T", as.numeric(rownames(ts))-1)
  
  # assuming the metrics are cumulative, get the metric
  target_ranges = ts %>%
    group_by(group) %>%
    dplyr::filter(target == max(target)) %>%
    ungroup(.) %>%
    # in case there are more than one maximums, get the most recent.
    tail(x = ., n = 1) %>%
    mutate(lb_start_deviation = target * lb_start_mult,
           ub_start_deviation = target * ub_start_mult,
           lb_stop_deviation = target * lb_stop_mult,
           ub_stop_deviation = target * ub_stop_mult) %>%
    dplyr::select(group, lb_start_deviation, ub_start_deviation, lb_stop_deviation, ub_stop_deviation)
  
  
  # These multipliers allows us to give less weigh to the earlier observations and more weigh
  # To the latest observations for the initial bounds.
  target_ranges_multipliers = seq.default(from = 3, to = 1, length.out = nrow(ts))
  
  ts = ts %>%
    left_join(target_ranges, by = "group") %>%
    mutate(lower_bounds_start = ifelse(target == 0, 1e-3,pmax(target - target_ranges_multipliers*lb_start_deviation, 1)),
           upper_bounds_start = ifelse(target == 0, 20000,pmax(target + target_ranges_multipliers*ub_start_deviation, 1)),
           lower_bounds_stop =  ifelse(target == 0, 1e-3, pmax(target - lb_stop_deviation*target_ranges_multipliers, 1) ),
           upper_bounds_stop =  ifelse(target == 0, 10000,pmax(target + ub_stop_deviation*target_ranges_multipliers, 1) )) %>%
    dplyr::select(group, name, target, lower_bounds_start, upper_bounds_start, lower_bounds_stop, upper_bounds_stop)
  
  return(ts)
}





# Target Function - Runs the Model for a single run -----------------------

# Handle computations
covid.target.fun <-  function(x, model, location_ids, minR0, maxR0, model_outcomes, roll_avg=30, lower_bounds, upper_bounds){
  
  require(dplyr)
  require(cli)
  
  # This function
  run_id = 1
  
  # Get parameters
  model_parameters = model$parameter %>% filter(Model == model$model_name)
  
  # remove constant priors (must do this to avoid singular covariance matrix)
  model_parameters_priors   <- model_parameters %>% filter(MinCalibrationValue != MaxCalibrationValue)
  model_parameters_constant <- model_parameters %>% filter(MinCalibrationValue == MaxCalibrationValue) %>% select(InternalName, ReferenceValue)
  
  param_df <- as.data.frame(t(x))
  names(param_df) <-c(model_parameters_priors$InternalName)
  
  df1 <- as.data.frame(t(model_parameters_constant))
  names(df1) <- as.character(unlist(df1["InternalName",]))
  df1 <- df1[2,]  
  
  df.full <- lapply(df1, function(x) as.numeric(as.character(x))) %>% as.data.frame()  # convert to numeric
  
  
  # bind
  param_df_full <- cbind(param_df , df.full) %>% mutate(ScenarioID = run_id)
  #-------------------------------------------------------------------------------------#
  
  model = model %>%  set_calibration_dates(.) %>% run_exponential_regressions(.) 
  
  # Determining the Calibratin date as a function of the available data:
  calibration_date = lubridate::as_date(model$calibration_date)
  start_date       = lubridate::as_date(model$start_date)
  init_num_calibration_days  = as.integer(calibration_date - start_date + 1)
  past_data_calibration_days = as.integer(lubridate::as_date(model$max_ts_date) - start_date + 1)
  
  
  # Initiation number is set here manually ----------------------------------
  model$initiation_numbers = model$location %>%
    select(LocationID, Name, Population) %>%
    mutate(InitiationNum = 0.0001) %>%
    mutate(InitiallyInfected = Population * InitiationNum)
  
  
  model = model %>%  set_time(.,final_time = past_data_calibration_days) %>% 
    set_calibrated_k_imabc(., location_ids = location_ids, parameters_df = param_df_full, minR0 = minR0, maxR0 = maxR0)
  
  calibration_results = calibrate_params_lhs_imabc(run_id = run_id, model = model)
  
  # Average model outcomes, and group them as targets
  res.named <- c()
  for(i in 1:length(model_outcomes)){  # set the multipliers for each target, consider another list of lists
    vec <- colMeans(matrix(calibration_results[,model_outcomes[i]], nrow=roll_avg)) %>% as.data.frame() %>% rename(metric = ".") 
    for(j in 1:nrow(vec)){
      res.named[paste0("G",i, "_T", j-1)] <- vec[j, "metric"] * model$location$Population[model$location$LocationID == location_ids]
    }
  }
  
  res.order <- res.named[order(names(res.named))]
  
  return(res.order)
  
}


# Backend Function - responsible for running the model in parallel --------

backend_fun_covid <- function(parms_to_run, all_parm_names, target_fun, other_inputs){
  
  combined_results = function(...) {
    as.data.frame(rbind(...))
  }
  
  # pass relevant functions to cores
  res <- foreach(i1 = 1:nrow(parms_to_run), .combine = combined_results, 
                 .export = 
                   c("set_time", "filter_inputs", "set_calibration_dates", "set_calibrated_k_imabc", 
                     "calibrate_initiation_num", "run_exponential_regressions",
                     "set_selected_params_for_run_id", "solve_model", "safe_division", "cat_green_tick", "calibrate_params_lhs_imabc", 
                     "location_ids", "maxR0", "minR0")) %dopar% { 
                       
                       inp <- unlist(as.data.frame(parms_to_run)[i1, all_parm_names])
                       
                       # Simulate targets
                       sim_target <- target_fun(inp, model=model, location_ids=location_ids, minR0=minR0, maxR0=maxR0, 
                                                model_outcomes = c("CumulativeDeaths", "Susceptible"),
                                                lower_bounds = other_inputs$targets$current_lower_bounds, 
                                                upper_bounds = other_inputs$targets$current_upper_bounds)
                       
                       return(sim_target)
                       
                     }
  
  print("Finished calculating the simulated targets")
  return(res)
}


# Set Calibrated K function - specialized for IMABC -----------------------

set_calibrated_k_imabc = function(model, location_ids, parameters_df,  minR0 = 2, maxR0 = 4){
  
  # Run the get param function, compute tau for each scenario.
  model$scenarios <- parameters_df
  model <- model %>% model$set_computed_params(.)
  
  runs_locations = as.data.frame(expand.grid(model$scenarios[,"ScenarioID"], location_ids))
  names(runs_locations) = c("ScenarioID", "LocationID")
  
  # Joining the data we need:
  model$scenarios = runs_locations %>% left_join(as.data.frame(model$scenarios), by = "ScenarioID") %>%   left_join(model$location, by = "LocationID")
  model$scenarios$RunID = 1:nrow(model$scenarios)
  
  # Defining the minimum and maximum population density
  minPopDensity = min(model$location$PopulationDensity)
  maxPopDensity = max(model$location$PopulationDensity)
  
  MaxT0CoeffRSquared = model$scenarios$MaxT0CoeffRSquared[1]
  T0Coeff = model$scenarios$T0Coeff[1]
  
  # Computing R0 using either a regression or the baseline R0. # The gradient found in the regression is used to the extent to which it fits the data.  
  model$scenarios = model$scenarios %>%
    mutate(
      referenceR0 = minR0 + (maxR0 - minR0) * (PopulationDensity - minPopDensity) / (maxPopDensity - minPopDensity),
      R0_regression = T0Coeff * tau + 1,
      R0 = ifelse(MaxT0CoeffRSquared > 0.8, R0_regression, referenceR0),
      cbeta = R0 / tau.eff
    )
  
  # For now, doing this only for the No Intervention Matrix: # For Age Strata models, let's compute cbeta k:
  compute_cbeta_pop = function(location_id, model) {
    pop_distribution = model$compartment$PopulationShareInLocation[model$compartment$LocationID == location_id]
    cbeta_pop = t(pop_distribution) %*% model$cbetamatrices[[1]] %*% pop_distribution
    cbeta_pop
  }
  
  cbeta_pop_location = data.frame(LocationID = location_ids, cbeta_pop = sapply(location_ids, compute_cbeta_pop, model = model))
  
  model$scenarios = model$scenarios %>%  left_join(cbeta_pop_location, by = "LocationID") %>%  mutate(k = R0/(tau.eff*cbeta_pop))
  
  model
}



# This functino runs a specific model run_id ------------------------------

calibrate_params_lhs_imabc = function(run_id, model, calibration_day) {
  
  locationid = model$scenarios$LocationID[run_id]
  
  initiation.num = model$initiation_numbers$InitiationNum[model$initiation_numbers$LocationID == locationid]
  
  final_inputs = model %>% filter_inputs(., location_ids = locationid, level = model$level)
  
  results = final_inputs %>%
    set_selected_params_for_run_id(.,run_id) %>%
    model$set_initial_stocks(.,initial_exposed = initiation.num) %>%
    model$set_stocks_positions(.) %>%
    solve_model(., run_id = run_id)
  
  results$LocationID = locationid
  
  results
}



# Pert density function ---------------------------------------------------

dpert = function(p, x.min, x.mode=NULL, x.max,lambda = 4, mu=NULL){
  # See http://www.riskamp.com/beta-pert
  if(!is.null(mu) & is.null(x.mode)) {
    x.mode<- (mu*( lambda + 2 )- (x.min + x.max))/lambda
    if(x.mode>x.max) x.mode <- min(mu,x.max)
    if(x.mode<x.min) x.mode <- x.min
  }
  if(is.null(mu) & is.null(x.mode)) stop( "invalid parameters" );
  if( x.min > x.max || x.mode > x.max || x.mode < x.min ) stop( "invalid parameters" );
  
  x.range <- x.max - x.min;
  if( x.range == 0 ) return( rep( x.min, n ));
  
  mu <- ( x.min + x.max + lambda * x.mode ) / ( lambda + 2 );
  
  # special case if mu == mode
  if( abs(mu - x.mode)/x.mode < 0.00001 ){
    v <- ( lambda / 2 ) + 1
  }else {
    v <- (( mu - x.min ) * ( 2 * x.mode - x.min - x.max )) /
      (( x.mode - mu ) * ( x.max - x.min ));
  }
  w <- ( v * ( x.max - mu )) / ( mu - x.min );
  
  res <- dbeta( ((p - x.min)/x.range), v, w )
  
  if(res == 0){
    return(1) 
  } else {
    if(is.na(res)){
      return(1)
    }
    return(res)
  }
  
}
