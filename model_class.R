
#------------------------------------------------------------------------------#
# Code for the paper: Reopening California
#                     Seeking Robust, Non-Dominated COVID-19 Exit Strategies.
# 
# Author: Pedro Nascimento de Lima
# Copyright (C) 2021 by The RAND Corporation
# See LICENSE.txt and README.txt for information on usage and licensing
#------------------------------------------------------------------------------#


#### COVID-19 Model Class AMSH ---------------------------------------------####

#------------------------------------------------------------------------------#
# Authors: Raffaele Vardavas, Pedro Nascimento de Lima
# Purpose: This File contains the c19model2 functions.
# Creation Date: May 2020
#------------------------------------------------------------------------------#

 
#' C19 Model 4 Function
#'
#' @param inputs inputs object created with the get_augmented_inputs () and prepare_inputs() functions
#' @param level use "state" as default.
#'
#' @return a c19model object
#' @export
#'
#' @examples
#'\dontrun{
#'model = get_augmented_inputs(...) %>%
#'  prepare_inputs(...) %>%
#'  c19model_vamshi4s(., level = "state")
#'}
#'
c19model_vamshi4s = function(inputs, level) {
  
  model = inputs %>%
    uniformize_inputs_level(.,level = level) %>%
    set_capacities(.)
  # Initiate Covid-19 model as an empty class.
  # Defining the model geographic level:
  
  model$level = level
  
  model$model_fn = model_function.c19model_vamshi4s
  
  model$set_calibrated_stocks = set_calibrated_stocks.c19model_vamshi4s
  
  model$set_initial_stocks = set_initial_stocks.c19model_vamshi4s
  
  model$set_computed_params = set_computed_params.c19model_vamshi4s
  
  model$pre_compute = pre_compute_function.c19model_vamshi4s
  
  model$verify_input_parameters = verify_input_parameters.c19model_vamshi4s
  
  model$set_stocks_positions = set_stocks_positions.c19model_vamshi4s
  
  model$model_name = "c19model_vamshi4s"
  
  model$status = "uncalibrated"
  
  # Uniformize Inputs according to the intended level:
  
  class(model) = c("c19model_vamshi4s", "c19model")
  
  model
}

# use this in a near future:
set_calibrated_stocks.c19model_vamshi4s = function(model, run_id) {
  
  cal_results = model$cal_results %>%
    filter(RunID == run_id) %>%
    filter(time == max(time))
  
  stocks_list = c(
    RB = unname(cal_results[,model$spos$RB+1]),
    S = unname(cal_results[,model$spos$S+1]),
    V = unname(cal_results[,model$spos$V+1]),
    E = unname(cal_results[,model$spos$Ev+1]),
    Ev = unname(cal_results[,model$spos$E+1]),
    P = unname(cal_results[,model$spos$P+1]),
    Pv = unname(cal_results[,model$spos$Pv+1]),
    ISm = unname(cal_results[,model$spos$ISm+1]),
    YSm = unname(cal_results[,model$spos$YSm+1]),
    ISs = unname(cal_results[,model$spos$ISs+1]),
    YSs = unname(cal_results[,model$spos$YSs+1]),
    H = unname(cal_results[,model$spos$H+1]),
    ICU = unname(cal_results[,model$spos$ICU+1]),
    IA = unname(cal_results[,model$spos$IA+1]),
    IAv = unname(cal_results[,model$spos$IAv+1]),
    YA = unname(cal_results[,model$spos$YA+1]),
    YAv = unname(cal_results[,model$spos$YAv+1]),
    D = unname(cal_results[,model$spos$D+1]),
    RS = unname(cal_results[,model$spos$RS+1]),
    RA = unname(cal_results[,model$spos$RA+1]),
    RAv = unname(cal_results[,model$spos$RAv+1]),
    CumulativeRealCases = unname(cal_results[,model$spos$CumulativeRealCases+1]),
    CumulativePositiveTests = unname(cal_results[,model$spos$CumulativePositiveTests+1]),
    CumulativeTotalTests = unname(cal_results[,model$spos$CumulativeTotalTests+1]),
    CumulativeReportedRecovered = unname(cal_results[,model$spos$CumulativeReportedRecovered+1]),
    CumulativeReportedDeaths = unname(cal_results[,model$spos$CumulativeReportedDeaths+1]),
    CumulativeVaccinated = unname(cal_results[,model$spos$CumulativeVaccinated+1]),
    TestingCapacity = cal_results$TestingCapacity,
    BedsCapacity = cal_results$BedsCapacity,
    VentilatorCapacity = cal_results$VentilatorCapacity,
    TreatmentEfficacy = cal_results$TreatmentEfficacy,
    #Initialize at NPI Level 3 - which is the current NPI level.
    NPIStock = cal_results$NPIStock,
    VaccineStock = cal_results$VaccineStock
  )
  
  model$init_stocks = unlist(stocks_list)
  
  ### Making sure all stocks are positive:
  #model$init_stocks = model$init_stocks + 1e-10
  
  model
  
}


#----------------------------------------------------------------------------------------#
# Function: Set Initial Stocks
# Purpose: Sets the Value of Initial Stocks
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#

set_initial_stocks.c19model_vamshi4s = function(model, initial_exposed) {
  
  # Defining Initial Conditions for Each Stock:
  stocks_list = c(
    RB = model$compartment$PopulationShareInLocation * 0,
    S = model$compartment$PopulationShareInLocation * (1-initial_exposed),
    V = rep(0, length(model$compartment$PopulationShareInLocation)), # model$compartment$PopulationShareInLocation, # TODO: Testing 
    E = model$compartment$PopulationShareInLocation * (1/3) * initial_exposed,
    Ev = model$compartment$PopulationShareInLocation * 0,
    P = model$compartment$PopulationShareInLocation * (1/3) * initial_exposed,
    Pv = model$compartment$PopulationShareInLocation * 0,
    ISm = model$compartment$PopulationShareInLocation * 0,
    YSm = model$compartment$PopulationShareInLocation * 0,
    ISs = model$compartment$PopulationShareInLocation * 0,
    YSs = model$compartment$PopulationShareInLocation * 0,
    H = model$compartment$PopulationShareInLocation * 0,
    ICU = model$compartment$PopulationShareInLocation * 0,
    IA = model$compartment$PopulationShareInLocation * (1/3) * initial_exposed,
    IAv = model$compartment$PopulationShareInLocation * 0,
    YA = model$compartment$PopulationShareInLocation * 0,
    YAv = model$compartment$PopulationShareInLocation * 0,
    D = model$compartment$PopulationShareInLocation * 0,
    RS = model$compartment$PopulationShareInLocation * 0,
    RA = model$compartment$PopulationShareInLocation * 0,
    RAv = model$compartment$PopulationShareInLocation * 0,
    CumulativeRealCases = model$compartment$PopulationShareInLocation * 0,
    CumulativePositiveTests = model$compartment$PopulationShareInLocation * 0,
    CumulativeTotalTests = model$compartment$PopulationShareInLocation * 0,
    CumulativeReportedRecovered = model$compartment$PopulationShareInLocation * 0,
    CumulativeReportedDeaths = model$compartment$PopulationShareInLocation * 0,
    CumulativeVaccinated = model$compartment$PopulationShareInLocation * 0,
    TestingCapacity = model$capacities$MinTestingCapacity[model$capacities$LocationID == model$location$LocationID],
    BedsCapacity = model$capacities$MinBedsCapacity[model$capacities$LocationID == model$location$LocationID],
    VentilatorCapacity = model$capacities$MinVentilatorCapacity[model$capacities$LocationID == model$location$LocationID],
    TreatmentEfficacy = 0,
    NPIStock = 1,
    VaccineStock = 0
  )
  
  ### Testing Multiplying Stock by big number to gain some precision.
  
  model$init_stocks = unlist(stocks_list)
  
  ### Making sure all stocks are positive:
  #model$init_stocks = model$init_stocks + 1e-10
  
  model
}


#----------------------------------------------------------------------------------------#
# Function: Set Computed Parameters
# Purpose: Computes model parameters based on scenarios input data.frame
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#

set_computed_params.c19model_vamshi4s = function(model) {
  
  new_scenarios = within(data = as.data.frame(model$scenarios), {
    
    ##############################################################################################
    ###                                                                                        ###
    ###   Rate from non infectious incumbation to infectious incubation phase                  ###
    ###                                                                                        ###
    ##############################################################################################
    
    nu = 1 / (d.incum * incum.non.infec.proportion)
    
    ##############################################################################################
    ###                                                                                        ###
    ###   Rates to first sympotoms
    ###                                                                                        ###
    ##############################################################################################
    
    d.to.first.sym =  (d.incum * (1 - incum.non.infec.proportion))
    
    ##############################################################################################
    ###                                                                                        ###
    ###   Rates to secondary phases mild sympotoms and anymptomatic
    ###                                                                                        ###
    ##############################################################################################
    
    gammaA = Asym.prop / d.to.first.sym
    
    gammaS = (1 - Asym.prop) / d.to.first.sym
    
    ##############################################################################################
    ###                                                                                        ###
    ###   Vaccination
    ###                                                                                        ###
    ##############################################################################################
    
    behavioral.increase.mixing = behavioral.increase.mixing
    
    v.rate = v.rate
    
    vacc.trans.efficacy = trans.efficacy.factor * vacc.overall.efficacy
    
    Asym.prop.vacc = 1 - (1 - vacc.overall.efficacy) * (1 - Asym.prop) / (1 - vacc.trans.efficacy)
    
    gammaAv = Asym.prop.vacc / d.to.first.sym
    
    gammaSv = (1 -  Asym.prop.vacc) / d.to.first.sym
    
    ##############################################################################################
    ###                                                                                        ###
    ###   Rates to develop severe symptoms or recover without developing severe sympotoms.
    ###                                                                                        ###
    ##############################################################################################
    
    upsilon = Severe.prop / d.sym.mild
    
    xi.m = 1 / d.sym.mild - upsilon
    
    ##############################################################################################
    ###                                                                                        ###
    ###   Rates to access the hospital if it is not at Capacity
    ###                                                                                        ###
    ##############################################################################################
    
    h = 1 / d.to.hos
    
    h.star = tested.multi.rate.access.to.hospital * h # faster rate for those tested first.
    
    
    ##############################################################################################
    ###                                                                                        ###
    ###   ICU dwelling time
    ###                                                                                        ###
    ##############################################################################################
    
    d.icu = d.hos * d.icu.mult
    
    ##############################################################################################
    ###                                                                                        ###
    ###   Rates to access ICU and to either die in the ICU or exit ICU.
    ###                                                                                        ###
    ##############################################################################################
    
    # Rates of death and recovery for those hospitalized.
    
    chi = Critical.prop / (d.hos - Critical.prop * d.icu)
    
    mu.ICU =  Die.in.icu.prop / d.icu
    
    xi.ICU = (1 - Die.in.icu.prop) / d.icu
    
    ##############################################################################################
    ###                                                                                        ###
    ###   Rates to exit the Hospital either due to death (not in ICU) or Recover
    ###                                                                                        ###
    ##############################################################################################
    
    # Rate of Death while hospitalized but not in ICU (including because the ICU is at capacity)
    mu.H  = (1 - A.icu) * chi #+ (Die.in.hos.prop - A.icu * Critical.prop * Die.in.icu.prop) /
      #((d.hos -  A.icu * Critical.prop * d.icu) * (1 - A.icu *Critical.prop))
    
    xi.H = (1 - Critical.prop) / (d.hos -  A.icu * Critical.prop * d.icu) - mu.H
    
    #chi = A.icu * chi
    
    ##############################################################################################
    ###                                                                                        ###
    ###   Rates of Death and Decovery for those with Severe sympotoms but not hospitalized
    ###                                                                                        ###
    ##############################################################################################
    
    ## Death rates for those with severe sympotoms that do not get hospitalized.
    
    d.to.death.if.not.hos  =  (d.to.hos + d.hos - Critical.prop * d.icu)
    
    xi.s = (1 - A * Hosp.prop) * (1 - Critical.prop) / d.to.death.if.not.hos
    
    mu.s =  (1 - A * Hosp.prop) * Critical.prop / d.to.death.if.not.hos
    
    ##############################################################################################
    ###   Rate to recovery for those asympotomatic
    ###                                                                                        ###
    ##############################################################################################
    
    xi.A = 1 / d.asym
    
    ##############################################################################################
    ###                                                                                        ###
    ###   R0 and the time scales
    ###                                                                                        ###
    ##############################################################################################
    
    # dummy parameters to express R0.
    
    a = mu.H + xi.H
    
    b = xi.m + upsilon
    
    c = A * h + mu.H + xi.s
    
    ## Calculate tau to get R0 from growth rate
    
    num1 = a * (gammaA * b * c + xi.A * (b * c + 1 * gammaS * c + upsilon * 1 * gammaS))
    
    num2 = xi.A * A * h * upsilon * 1 * gammaS
    
    den = xi.A * (gammaA + gammaS) * a * b * c
    
    tau.inf = (num1 + num2) / den
    
    tau.exp = 1 / nu
    
    tau = tau.exp + tau.inf
    
    ## Calculate tau.eff to get cbeta
    
    num1 = a * (gammaA * b * c + xi.A * (b * c + m.Sm * gammaS * c + upsilon * m.Ss * gammaS))
    
    num2 = xi.A * A * h * upsilon * m.h * gammaS
    
    den = xi.A * (gammaA + gammaS) * a * b * c
    
    tau.eff = (num1 + num2) / den
    
  })
  
  
  ##############################################################################################
  ###                                                                                        ###
  ###   Output
  ###                                                                                        ###
  ##############################################################################################
  
  model$scenarios = new_scenarios
  
  model
  
}


#----------------------------------------------------------------------------------------#
# Function:  Verify Model Parameters.
# Purpose: Checks model parameters consistency
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#

verify_input_parameters.c19model_vamshi4s = function(model) {
  # verify Parametrers
  within(data = as.data.frame(model$scenarios), {
    verify.out <- c(
      "Vaccination Behaviorral Mixing Increase" = behavioral.increase.mixing,
      "Vaccination transmission efficacy" = vacc.trans.efficacy,
      "Vaccination rate" = v.rate,
      "E duration" = 1 / nu,
      "P duration" = 1 / (gammaA + gammaS),
      "Prop asymp" = gammaA / (gammaA + gammaS),
      "Prop asymp vaccinated" = gammaAv / (gammaAv + gammaSv),
      "Sympotomatic mild duration" = 1 / (upsilon + zetaS + xi.m),
      "Proportion getting severe" = upsilon / (upsilon + zetaS + xi.m),
      "Sympotomatic to Hospitalized duration" = 1 / h,
      "Hospital duration" = (1-Critical.prop)*1/(xi.H+mu.H)+A.icu*(Critical.prop)*d.icu,
      "Mortality of non Hosp" =  mu.H / (mu.H + zetaS + xi.s),
      "Mortality of Hosp" =  (1-Critical.prop)*mu.H / (mu.H + xi.H) + Critical.prop*mu.ICU / (mu.ICU + xi.ICU),
      "Mortality of ICU" =  mu.ICU / (mu.ICU + xi.ICU),
      "IA duration" = 1 / xi.A,
      "IA to tested duration" = 1 / zetaA,
      "generation time scale" = tau,
      "modified time scale for R0" = tau.eff
    )
  })
}


pre_compute_function.c19model_vamshi4s = function(model, run_id){
  
  # Substitute New Parameters:
  pc = list()
  
  # spmin is a smooth pmin function:
  # exponential smooth min (k = 32); see https://www.iquilezles.org/www/articles/smin/smin.htm
  pc$spmin<- function(a,b,k=320){
    res = exp( -k*a ) + exp( -k*b )
    res= pmin(res,1)
    return (-log( res )/k)
  }
  
  pc$spmax<- function(a,b,k=320){
    res = exp( k*a ) + exp( k*b )
    return (log( res )/k)
  }
  
  ### Start of Vaccination:
  pc$vaccination_start_date = lubridate::as_date("2021-01-01")
  
  pc$delta_t.vacc = 20
  
  pc$vacc_approved_for = c(0.1, rep(1, length(model$compartment$PopulationShareInLocation)-1)) # c(0.1,1,1,1,1,1,1)
  
  
  # Comes from the Experimental Design
  pc$prob_seek_vaccination = ifelse(exists("prob_seek_vaccination", where = model$params), model$params$prob_seek_vaccination, 1/2) 
  
  # Comes from the Experimental Design
  pc$vacc_on_demand = ifelse(exists("vacc_on_demand", where = model$params), model$params$vacc_on_demand, T) 
  
  
  ### Vaccination strategy Definition:
  frailty_vector = model$subpopulation$VaccPriority
  
  frailty_vector = frailty_vector / sum(frailty_vector)
  
  mixing_no_intervention_criteria_vector = safe_division(rowSums(model$cbetamatrices[[1]]), model$compartment$PopulationShareInLocation)
  
  mixing_no_intervention_criteria_vector = mixing_no_intervention_criteria_vector / sum(mixing_no_intervention_criteria_vector)
  
  mixing_essential_workers_criteria_vector = safe_division(rowSums(model$cbetamatrices[[4]]), model$compartment$PopulationShareInLocation)
  
  mixing_essential_workers_criteria_vector = mixing_essential_workers_criteria_vector / sum(mixing_essential_workers_criteria_vector)
  
  frailty_vector_sequential = 10^rank(frailty_vector) 
  
  mixing_essential_workers_vector_sequential = 10^rank(mixing_essential_workers_criteria_vector*pc$vacc_approved_for) 
  
  # Think about how to use the eigenvectors:
  eigen =  eigen(model$cbetamatrices[[1]])
  
  abs_eigen_vector = abs(eigen$vectors[,which.max(eigen$values)])
  
  vacc_criteria = abs_eigen_vector / sum(abs_eigen_vector)
  
  
  if(!exists("vaccination_strategy_name", where = model$params)) {
    #Change to Something Sensible:
    vaccination_strategy_name = "ACIP"
  } else {
    vaccination_strategy_name = as.character(model$params$vaccination_strategy_name) 
  }
  
  ## Implement allocation efficiency adjustment.
  
  pc$V.strategy = switch(vaccination_strategy_name, 
                         "Proportional" = model$compartment$PopulationShareInLocation,
                         "ACIP" = model$subpopulation$ACIPVaccPriority,
                         "Frailty-Based" = frailty_vector,
                         "Frailty-Based-Sequential" = frailty_vector_sequential,
                         "Mixing-Based-No-Intervention" =  mixing_no_intervention_criteria_vector,
                         "Mixing-Based-Essential-Workers" = mixing_essential_workers_criteria_vector,
                         "Mixing-Based-Essential-Workers-Sequential" = mixing_essential_workers_vector_sequential,
                         "No Vaccine" =  model$compartment$PopulationShareInLocation * 0
  )
  
  ### Start of Treatment Improvement
  # If Date is greater than the day in which this remdesevir study was published, 
  # start improving treatments:
  # https://www.nejm.org/doi/full/10.1056/NEJMoa2007764
  pc$treatment_improvement_start_date = lubridate::as_date("2020-05-22")
  
  
  
  ##############################################################################################
  ###                                                                                        ###
  ###   Adaptive Strategies Flags
  ###                                                                                        ###
  ##############################################################################################
  
  
  # Check which NPI Policies Should be Evaluated
  
  pc$evaluate_fixed_npis = exists("npi_type", where = model$params) & exists("fixed_npi", where = model$params)
  
  pc$evaluate_intermittent_npis = exists("npi_type", where = model$params) & exists("base_npi", where = model$params) & exists("max_npi", where = model$params) & exists("npi_period", where = model$params)
  
  pc$evaluate_adaptive_npis = exists("npi_type", where = model$params) & exists("LevelOfCaution", where = model$params)
  
  pc$evaluate_adaptive_time_based_npis = pc$evaluate_adaptive_npis & exists("TransitionDate", where = model$params) & exists("NewLevelOfCautionMultiplier", where = model$params)
  
  pc$evaluate_adaptive_vacc_based_npis = pc$evaluate_adaptive_npis & exists("AdaptiveCautionRelaxationRate", where = model$params) & exists("AdaptiveCautionMidpoint", where = model$params) & exists("reopening_criteria", where = model$params)
  
  pc$evaluate_immunity_npi_threshold = exists("reopening_immunity_threshold", where = model$params) & exists("reopening_criteria", where = model$params)
  
  
  ##############################################################################################
  ###                                                                                        ###
  ###   Seasonality
  ###                                                                                        ###
  ##############################################################################################
  
  # Below is the function for seasonality - it will be replaced.
  pc$seasonal_function = function(seas, t) {
    (seas * (1 - (sin(3.14 * (t+seas_shift) / 365)) ^ 2) + 1)
  }
  
  # Compute Seasonal Factor for Today:
  
  # First, Compute a Baseline seasonal factor reference:
  
  initial_t = as.integer(model$start_date - lubridate::as_date("2019-12-15"))
  
  max_ts_t =  as.integer(model$max_ts_date - lubridate::as_date("2020-12-15"))
  
  # t_model = t + initial_t - 1
  
  t_model = model$time + initial_t - 1
  
  pc$seasonal_factors = pc$seasonal_function(model$params$seas, t_model) / mean(pc$seasonal_function(model$params$seas, initial_t:max_ts_t)) 
  
  pc$baseline_seasonal_factor = mean(pc$seasonal_function(model$params$seas, initial_t:max_ts_t)) 
  
  ## Computting the Aggregate CB
  
  ## Any exogenous influence on transmission should be defined below:
  pc$transmissibility_multiplier = ifelse(exists("PercentChangeInTransmissibility", where = model$params), (1+model$params$PercentChangeInTransmissibility),1)
  
  # Changing Age-dependent Parameters:
  # CHange Vaccine Efficacy if it is an experimental parameter:
  # Assume efficiency factors are present.
  
  
  # Computing parameters that rely on multipliers that may or may not be supplied:
  
  # We might want to generalize this for any parameter, but one needs to be careful
  # since there are parameters that are computed in the set_computed_params
  
  # mult.trans.efficacy.factor
  mult.trans.efficacy.factor = ifelse(exists("mult.trans.efficacy.factor", where = model$params), model$params$mult.trans.efficacy.factor,1)
  
  vacc.trans.efficacy = mult.trans.efficacy.factor * model$params$trans.efficacy.factor * model$params$vacc.overall.efficacy
  
  pc$Asym.prop.vacc = 1 - (1 - model$params$vacc.overall.efficacy) * (1 - model$params$Asym.prop) / (1 - vacc.trans.efficacy)
  
  
  # mult.vrate
  mult.v.rate = ifelse(exists("mult.v.rate", where = model$params), model$params$mult.v.rate,1)
  pc$v.rate = model$params$v.rate * mult.v.rate
  
  # Behavioral increase in mixing: mult.behavioral.increase.mixing
  mult.behavioral.increase.mixing = ifelse(exists("mult.behavioral.increase.mixing", where = model$params), model$params$mult.behavioral.increase.mixing,1)
  pc$behavioral.increase.mixing = model$params$behavioral.increase.mixing * mult.behavioral.increase.mixing
  
  # Willingness to Vaccinate:
  mult.V.will.factor = ifelse(exists("mult.V.will.factor", where = model$params), model$params$mult.V.will.factor,1)
  pc$V.will.factor = model$params$V.will.factor * mult.V.will.factor
  
  # Months of Immunity Duration
  mult.MonthsOfImmunityDuration = ifelse(exists("mult.MonthsOfImmunityDuration", where = model$params), model$params$mult.MonthsOfImmunityDuration,1)
  pc$MonthsOfImmunityDuration = model$params$MonthsOfImmunityDuration * mult.MonthsOfImmunityDuration
  
  
  # Compute start of the simulation run:
  pc$initial_time = Sys.time()
  
  
  # Return Pre-computed objects
  model$pc = pc
  
  # Return Model
  model
  
}




#----------------------------------------------------------------------------------------#
# Function:  Model Function
# Purpose: This function contains the model equations, and represent the actual ODE model.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#
#' @importFrom utils tail
model_function.c19model_vamshi4s = function(time, stocks, params, model, run_id) {
  
  # Substituting timeseries Nans with zeros:
  
  # Exporting Time Series Objects
  if (length(time) == 1) {
    # Here I select the Timeseries object using the row number. This could be changed so that the time series is selected using a date.
    if(time < nrow(model$timeseries)) {
      ts = list(ts = model$timeseries[as.integer(time),])
    } else {
      ts = list(ts = tail(model$timeseries, 1))
    }
  } else {
    browser()
    ts = list()
  }
  
  t = list(t = time)
  
  with(
    as.list(c(params, model, stocks, ts, t)),
    {
      
      # All stocks are positive values:
      # stocks = abs(stocks)
      
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Unpacking Stocks
      ###                                                                                        ###
      ##############################################################################################
      
      RB = stocks[spos$RB]
      S = stocks[spos$S]
      V = stocks[spos$V]
      E = stocks[spos$E]
      Ev = stocks[spos$Ev]
      P = stocks[spos$P]
      Pv = stocks[spos$Pv]
      ISm = stocks[spos$ISm]
      YSm = stocks[spos$YSm]
      ISs = stocks[spos$ISs]
      YSs = stocks[spos$YSs]
      H = stocks[spos$H]
      ICU = stocks[spos$ICU]
      IA = stocks[spos$IA]
      IAv = stocks[spos$IAv]
      YA = stocks[spos$YA]
      YAv = stocks[spos$YAv]
      D = stocks[spos$D]
      RS = stocks[spos$RS]
      RA = stocks[spos$RA]
      RAv = stocks[spos$RAv]
      CumulativeRealCases = stocks[spos$CumulativeRealCases]
      CumulativePositiveTests = stocks[spos$CumulativePositiveTests]
      CumulativeTotalTests = stocks[spos$CumulativeTotalTests]
      CumulativeReportedRecovered = stocks[spos$CumulativeReportedRecovered]
      CumulativeReportedDeaths = stocks[spos$CumulativeReportedDeaths]
      CumulativeVaccinated = stocks[spos$CumulativeVaccinated]
      TestingCapacity = stocks[spos$TestingCapacity]
      BedsCapacity = stocks[spos$BedsCapacity]
      VentilatorCapacity = stocks[spos$VentilatorCapacity]
      TreatmentEfficacy = stocks[spos$TreatmentEfficacy]
      NPIStock = stocks[spos$NPIStock]
      VaccineStock = stocks[spos$VaccineStock]
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Verification Browser
      ###                                                                                        ###
      ##############################################################################################
      
      # if(any(is.nan(stocks))) {
      #   browser()
      # }
      
      #
      # if(any(stocks<0)) {
      #   browser()
      # }
      
      # if(ts$Date == lubridate::as_date("2020-03-01")){
      #   browser()
      # }
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Portfolios
      ###                                                                                        ###
      ##############################################################################################
      
      date = floor(t) + start_date - 1
      
      # NPI Portfolio is defined by the NPI Stock continuous variable:
      npi_portfolio = unname(round(NPIStock, digits = 0))  
      
      # The Target NPI Portfolio is the variable used to Change the NPI stock. By default, it comes from the timeseries:
      
      # The target portfolio can change following the NPi strategies rules later:
      target_npi_portfolio = ts$PortfolioID
      
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Option 0 - Fixed NPIs
      ###                                                                                        ###
      ##############################################################################################
      
      # Check if all parameters for intermittent NPIs are in place:
      
      
      # Uncomment if You want to run Periodic NPIs:
      
      if(pc$evaluate_fixed_npis) {
        
          if(as.character(npi_type) == "fixed") {
            target_npi_portfolio = fixed_npi
          } 
        
      }
      
      
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Option 1 - Intervention Schedule Originally used in the COVID-19 State Policy Tool
      ###                                                                                        ###
      ##############################################################################################
      
      # 
      # npi_portfolio = if(!exists("intervention_schedule") || date <= calibration_date) {
      # 
      #   ts$PortfolioID
      # 
      # } else {
      # 
      #   # There is an intervention schedule and we are simulating the future:
      #   tentative_npi = intervention_schedule %>%
      #     mutate(Passed = date >= ChangeDates) %>%
      #     filter(Passed) %>%
      #     filter(ChangeDates == max(ChangeDates))
      # 
      #   # If this doesnt return any date, then use the final time series object, otherwise, use the portfolio id coming from the past timeseries.
      #   ifelse(nrow(tentative_npi) == 1, as.integer(tentative_npi$SimulatedPortfolioID), ts$PortfolioID)
      # 
      # }
      
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Option 2 - Intermittent NPIs varying according to some periodicity
      ###                                                                                        ###
      ##############################################################################################
      
      # Check if all parameters for intermittent NPIs are in place:
      
      
      # Uncomment if You want to run Periodic NPIs:

      if(pc$evaluate_intermittent_npis) {
        
        if(date >= lubridate::as_date(periodic_npi_start_date)) {
          
          if(as.character(npi_type) == "periodic") {
            target_npi_portfolio = base_npi + (max_npi - base_npi) * (round(t / npi_period) %% 2)
          } 
          
        }
        
      }
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Option 3 - Adaptive NPIs:
      ###                                                                                        ###
      ##############################################################################################
      
      # Computing the Non-susceptible population - this is used in the adaptive-imunity-based npi strategies
      # and in the reopening condition. 
      
      if(pc$evaluate_adaptive_npis) {
        
        KnownPrevalence =  sum(YSs + YSm + YA + ICU + H + YAv)
        
        # First, try to evaluate time-based adaptive NPIs:
        if(pc$evaluate_adaptive_time_based_npis) {
          
          if(npi_type == "adaptive-time-based") {
            
            if(date > lubridate::as_date(TransitionDate)) {
              target_npi_portfolio = round(min(KnownPrevalence * 1000 * LevelOfCaution * NewLevelOfCautionMultiplier, 5), digits = 0) + 1  
            } else {
              target_npi_portfolio = round(min(KnownPrevalence * 1000 * LevelOfCaution, 5), digits = 0) + 1
            } 
            
          }
          
          
        }
        
        
        # Then, try to evaluate immunity-based adaptive npis:
        if(pc$evaluate_adaptive_vacc_based_npis) {
          
          if(npi_type == "adaptive-vacc-based") {
            
            # The immunity multiplier follows a logistic curve (reversed S-shaped) where:
            # Values range between 0 and 1.
            # AdaptiveCautionMidpoint [Should be between ~0.3 and a herd immunity threshold] determines the point at which the multiplier is equal to 0.5 (that is, level of caution is halved).
            # AdaptiveCautionRelaxationRate determines how fast we relax our NPIs based on vaccination. [10 ~ 40]
            
            cumulative_vaccinated_metric = ifelse(reopening_criteria=="All Population",sum(CumulativeVaccinated),(sum(CumulativeVaccinated[model$subpopulation$OldChronic])/sum(model$compartment$PopulationShareInLocation[model$subpopulation$OldChronic])))
            
            vacc_multiplier = 1-(1/(1+exp(-1*AdaptiveCautionRelaxationRate*(cumulative_vaccinated_metric-AdaptiveCautionMidpoint))))
            
            target_npi_portfolio = round(min(KnownPrevalence * 1000 * LevelOfCaution * vacc_multiplier, 5), digits = 0) + 1
            
          }
          
        }
        
      }
      
      
      # Formulation with the non-susceptible:
      all_population_non_susceptible = 1-sum(S)
      
      old_and_frail_non_susceptible = 1-(sum(S[model$subpopulation$OldChronic])/sum(model$compartment$PopulationShareInLocation[model$subpopulation$OldChronic]))
      
      # Formulation using the vaccinated:
      all_population_vaccinated = sum(CumulativeVaccinated)
      
      old_and_frail_vaccinated = (sum(CumulativeVaccinated[model$subpopulation$OldChronic])/sum(model$compartment$PopulationShareInLocation[model$subpopulation$OldChronic]))
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Option 4 - Reopen After Immunization threshold:
      ###                                                                                        ###
      ##############################################################################################
      
      if(pc$evaluate_immunity_npi_threshold) {
        
        # Using the non-susceptible:
        #criteria_metric_to_reopen = ifelse(reopening_criteria=="All Population",all_population_non_susceptible,old_and_frail_non_susceptible)
        
        
        # Or using the vaccinated:
        criteria_metric_to_reopen = ifelse(reopening_criteria=="All Population",all_population_vaccinated,old_and_frail_vaccinated)
        
        if(criteria_metric_to_reopen > reopening_immunity_threshold) {
          reopening_criteria_met = 1
          target_npi_portfolio = 1
        } else {
          reopening_criteria_met = 0
          # Target portfolio is unchanged.
        }
      } else {
        reopening_criteria_met = 0
      }
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Calculate the c * beta Infectivity and the effective R.
      ###                                                                                        ###
      ##############################################################################################
      
      # Original cbeta formula without calibration factor:
      
      # These calculations could also be done outside the model with some effort:
      
      cbeta_no_intervention = k * model$cbetamatrices[[1]]
      
      cbeta_from_highest_intervention = k * model$cbetamatrices[[ts$MaxPortfolioID]]
      
      calibrated_cbeta_highest_intervention <- cbeta_no_intervention/((npi.cal.factor*((cbeta_no_intervention/cbeta_from_highest_intervention)-1))+1)
      
      calibrated_cbeta_highest_intervention[is.nan(calibrated_cbeta_highest_intervention)] <- 0
      
      
      
      # Computing the CBeta for the ceiling:
      
      floor_npi = max(floor(NPIStock),1)
      
      ceiling_npi = min(ceiling(NPIStock),6)
      
      c_weight = NPIStock - floor(NPIStock)
      
      cbeta_with_intervention_c <- k * model$cbetamatrices[[ceiling_npi]]
      
      cbeta_with_intervention_f <- k * model$cbetamatrices[[floor_npi]]
      
      # For the Ceiling CBETA:
      calibrated_cbeta_with_intervention_f <- cbeta_no_intervention/((npi.cal.factor*((cbeta_no_intervention/cbeta_with_intervention_f)-1))+1)
      
      calibrated_cbeta_with_intervention_f[is.nan(calibrated_cbeta_with_intervention_f)] <- 0
      
      cbeta_f = calibrated_cbeta_highest_intervention * behavioral.adaptation.factor + (1-behavioral.adaptation.factor) * calibrated_cbeta_with_intervention_f
      
      
      # For the Floor CBETA:
      calibrated_cbeta_with_intervention_c <- cbeta_no_intervention/((npi.cal.factor*((cbeta_no_intervention/cbeta_with_intervention_c)-1))+1)
      
      calibrated_cbeta_with_intervention_c[is.nan(calibrated_cbeta_with_intervention_c)] <- 0
      
      cbeta_c = calibrated_cbeta_highest_intervention * behavioral.adaptation.factor + (1-behavioral.adaptation.factor) * calibrated_cbeta_with_intervention_c
      
      
      cbeta = cbeta_c * c_weight + cbeta_f * (1-c_weight)
      
      cbeta = matrix(pmax(0,cbeta),nrow=nrow(cbeta),ncol=ncol(cbeta))
      
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Seasonality
      ###                                                                                        ###
      ##############################################################################################
      
      # # Below is the function for seasonality - it will be replaced.
      # seasonal_function = function(seas, t) {
      #   (seas * (1 - (sin(3.14 * t / 365)) ^ 2) + (1 - seas))
      # }
      # 
      # # Compute Seasonal Factor for Today:
      # 
      # # First, Compute a Baseline seasonal factor reference:
      # 
      # initial_t = as.integer(start_date - lubridate::as_date("2020-01-01"))
      # 
      # max_ts_t =  as.integer(max_ts_date - lubridate::as_date("2020-01-01"))
      # 
      # t_model = t + initial_t - 1
      # 
      # seasonal_factor = seasonal_function(seas, t_model) / mean(seasonal_function(seas, initial_t:max_ts_t))
      # 
      # browser()
      
      # Options to use the seasonal factor:
      # A - Linear Interpolation at any point in time.
      
      
      # seasonal_factor = pc$seasonal_factors[which(floor(t)==time)] * (1-((t-floor(t)))) + (t-floor(t)) * pc$seasonal_factors[which(ceiling(t)==time)]
      
      # B - Without Linear interpolation:
      seasonal_factor = pc$seasonal_factors[which(floor(t)==time)]
      
      # C - Computing the seasonal factor on the fly:
      #seasonal_factor = pc$seasonal_function(seas, t) / pc$baseline_seasonal_factor
      
    
      # Cbeta with Seasonality computed internally:
      cbeta = cbeta * seasonal_factor * pc$transmissibility_multiplier
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Transmission Process
      ###                                                                                        ###
      ##############################################################################################
      
      # Transmission from the Susceptible:
      
      # Adding a pmin to ensure S_to_E is never greater than S:
      
      #TODO: Check this formula with Raff:
      
      S_to_E = cbeta %*% ((P + m.Sm * ISm + m.Ss * ISs +
                             m.tSm * YSm +  m.tSs * YSs +
                             m.h * (H + ICU) + IA + m.tA * YA +
                             pc$behavioral.increase.mixing * (Pv + IAv + m.tA * YAv)) *
                            S)
      
      
      # Asymptomatic infections:
      prop_asymp_infections = safe_division(sum(cbeta %*% ((P + IA + pc$behavioral.increase.mixing * (Pv + IAv)) * S)), sum(S_to_E))
      
      # Susceptibles are never completely depleted:
      conf_susceptibles = 0.8
      
      S_to_E = pc$spmin(S_to_E, S*conf_susceptibles)
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Transmission Process for the Vaccinated
      ###                                                                                        ###
      ##############################################################################################
      
      if(date >= (pc$vaccination_start_date + 1)) {
        
        V_to_Ev = (1-vacc.trans.efficacy) * pc$behavioral.increase.mixing * 
          cbeta %*% ((P + m.Sm * ISm + m.Ss * ISs +
                        m.tSm * YSm +  m.tSs * YSs +
                        m.h * (H + ICU) + IA + m.tA * YA +
                        pc$behavioral.increase.mixing * (Pv + m.Sm * IAv + m.tA * YAv)) *
                       V)
        
      } else {
        V_to_Ev = rep(0,length(compartment$PopulationShareInLocation))
      }
      
      # V to Ev is always positive.
      V_to_Ev = abs(V_to_Ev)
      
      
      
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Capacity Constraints - Testing, Hospitalization and ICU
      ###                                                                                        ###
      ##############################################################################################
      
      # First, we assume testing capacity is distributed according to the age distributions:
      # InfectedSympNotTestedByStrata = ISs + ISm
      # test_division_proportion = safe_division(InfectedSympNotTestedByStrata, sum(InfectedSympNotTestedByStrata))
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Hospital & ICU Capacity
      ###                                                                                        ###
      ##############################################################################################
      
      # Hospitalization Capacity Constraint Turned Off:
      A = 1
      
      # Uncomment the lines below to activate Hospitalization Capacity:
      # HospitalCapacityPerStrata = BedsCapacity * test_division_proportion
      
      # HospitalCapacityUtilizationRatio = safe_division(H+ICU, HospitalCapacityPerStrata)
      
      # Using a logistic eqn. to get a smooth transition from 1 to 0 and vice-versa for the solver to work.
      # A = 1 / (1+exp(10*(HospitalCapacityUtilizationRatio-1)))
      
      A.icu = 1
      
      #ICUCapacityPerStrata = VentilatorCapacity * test_division_proportion
      
      #ICUCapacityUtilizationRatio = safe_division(ICU, ICUCapacityPerStrata)
      
      # Using a logistic equn. to get a smooth transition from 1 to 0 and vice-versa for the solver to work.
      #A.icu = 1 /  (1+exp(10*(ICUCapacityUtilizationRatio-1)))
      
      
      ###################
      # Hospital InFlow 
      ###################
      
      ISs_to_H = h * A * ISs
      
      YSs_to_H = h.star * A * YSs
      
      H_to_ICU = A.icu * chi * H
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Re-evaluate parameters that depend on Hospital and ICU accessibility
      ###                                                                                        ###
      ##############################################################################################
      
      # Uncomment if using ICU Capacity and Hospitalization Capacity:
      
      mu.H  = subpopulation$Die.in.icu.prop.mult *
        ((1 - A.icu) * chi) #+ (Die.in.hos.prop - A.icu * Critical.prop * Die.in.icu.prop) /
           #((d.hos -  A.icu * Critical.prop * d.icu) * (1 - A.icu * Critical.prop)))

      xi.H =  ((1 - Critical.prop) / (d.hos -  A.icu * Critical.prop * d.icu) - mu.H)

      mu.s = subpopulation$Die.in.icu.prop.mult *
        ((1- A * Hosp.prop) * Critical.prop / d.to.death.if.not.hos)

      xi.s = ((1 - A * Hosp.prop) * (1 - Critical.prop) / d.to.death.if.not.hos)

      mu.ICU = mu.ICU * subpopulation$Die.in.icu.prop.mult
      
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Model Testing Progression InFlows based on Testing Capacity
      ###                                                                                        ###
      ##############################################################################################
      
      # Uncomment if using Testing Capacity:
      # TestingCapacityPerStrata = TestingCapacity * test_division_proportion
      
      # RemainingTests = max(0, sum(TestingCapacityPerStrata - ISs_to_H))
      # RemainingTestsPerStrata = RemainingTests * test_division_proportion
      
      # Then, we test severe cases:
      # With Testing Capacity Constraint:
      # ISs_to_YSs = pmin((1-A) * ISs * zetaS, RemainingTestsPerStrata)
      
      # Without Testing Capacity Constraint:
      ISs_to_YSs = (1-A) * ISs * zetaS
      
      # With Testing Capacity Constraint:
      #RemainingTests = sum(RemainingTestsPerStrata - ISs_to_YSs)
      #RemainingTestsPerStrata = RemainingTests * test_division_proportion
      
      # Then, we test mild cases:
      # With Testing Capacity Constraint:
      #ISm_to_YSm = pmin(zetaS * ISm, RemainingTestsPerStrata)
      
      # Without:
      ISm_to_YSm = zetaS * ISm
      
      
      #RemainingTests = sum(RemainingTestsPerStrata - ISm_to_YSm)
      #RemainingTestsPerStrata = RemainingTests * test_division_proportion
      
      # Then, we test asymptomatic cases:
      # With Testing Capacity Constraint:
      # IA_to_YA = pmin(zeta * IA, RemainingTestsPerStrata)
      IA_to_YA = zeta * IA
      
      #RemainingTests = sum(RemainingTestsPerStrata - IA_to_YA)
      #RemainingTestsPerStrata = RemainingTests * test_division_proportion
      
      # Fnally, we test asymptomatic cases that were vaccinated:
      # With Constraint:
      #IAv_to_YAv = pmin(zeta * IAv, RemainingTestsPerStrata)
      
      # Without:
      IAv_to_YAv = zeta * IAv
      
      #RemainingTests = sum(RemainingTestsPerStrata - IAv_to_YAv)
      
      #zeta = pmin(zeta, sum(safe_division(RemainingTests, (S + E + P + V + Ev + Pv))))
      
      #RemainingTests = sum(RemainingTestsPerStrata - zeta * (S + E + P + V + Ev + Pv)) 
      
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Compute the star parameters: time varying parameters due to variable tasting rates
      ###                                                                                        ###
      ##############################################################################################
      
      # Note the effective testing rates need to be higher than the biological progression rates to get the 
      # star progression rates (i.e., from diagnosed compartments). Otherwise mathematically the star rates 
      # goes to infinity. But we assume that nothing can transition in less than 12 hours - hence 2 days^-1.
    
      xi.A.star = diagnosed.increased.progression.rate * xi.A 

      xi.m.star = diagnosed.increased.progression.rate * xi.m

      upsilon.star =  diagnosed.increased.progression.rate * upsilon
      
      xi.s.star = xi.s
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Vaccination Capacity and Inflow
      ###                                                                                        ###
      ##############################################################################################
      
      # Assuming Vaccination Strategy Exists:
      vacc_allocation_efficiency = safe_division(S, (S+RA+RS))
      
      # Vaccination already started:
      if(date >= pc$vaccination_start_date) {
        
        # Vaccination Demand by Strata:
        # Current formulation
        # I suspect this formulation is causing discontinuities and causing the solver to halt because it is not continuous:
        
        # Original functino:
        
        population_willing_and_eligible = compartment$PopulationShareInLocation * pc$V.will.factor * pc$vacc_approved_for
        
        # TODO: Make the 100 a parameter for all logistic functions.
        
        # 100 seems fine, maybe the solver still won't like it, and we can decrease this number to move to other groups faster.
        # When we vaccinate 90% of the willing, that's when we shift our switching to the other groups to switch to the other groups
        any_willing_to_vaccinate = 1 / (1+exp(1000*(CumulativeVaccinated-population_willing_and_eligible*0.95)))
        
        vacc_demand = (S+RA+RS) * any_willing_to_vaccinate   
        
        vacc_demand_rate = pc$prob_seek_vaccination * vacc_demand 
        
        
        # Vaccination Capacity:
        # Actual vaccination rate is time varying and follows a logistic growth curve.
        vaccines_courses_delivered =  pc$v.rate * (1-1 / (exp((log(2)*as.numeric(date - pc$vaccination_start_date)/pc$delta_t.vacc))))
        
        actual_v.rate = VaccineStock * 1
        
        non_normalized_allocation = pc$V.strategy * any_willing_to_vaccinate

        # Vacc capacity Rate is the vaccination capacity by age strata, considering the allocation regime.
        # Maybe the line below should include a delivery constraint, so that not all doeses can be used in one day.
        vacc_capacity_rate = actual_v.rate * safe_division(non_normalized_allocation, sum(non_normalized_allocation))
        
        # Actual number of Vaccinated is the minimum between capacity and demand:
        dCumulativeVaccinated = pc$spmin(vacc_capacity_rate,vacc_demand_rate) 
        
        # The Actual Immunization rate considers the vaccination allocation efficiency by age group:
        S_to_V = dCumulativeVaccinated * vacc_allocation_efficiency
        
        # Tracking Doses Wasted:
        total_vaccines_available = actual_v.rate  #total_vaccines_available = sum(pc$v.rate * allocation) 
        
        vaccines_unused = total_vaccines_available - sum(dCumulativeVaccinated)
        
        
        dVaccineStock = vaccines_courses_delivered - sum(dCumulativeVaccinated)
        
        
        # if(date > lubridate::as_date(c("2021-12-15"))){
        #   browser()
        # }
      
      } else {
        S_to_V = rep(0,length(compartment$PopulationShareInLocation))
        dCumulativeVaccinated = rep(0,length(compartment$PopulationShareInLocation))
        total_vaccines_available = 0
        vaccines_unused = 0
        dVaccineStock = 0
      }
      
      
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Progression Flows
      ###                                                                                        ###
      ##############################################################################################
      
      ###################
      # Sym vs Asym Rates 
      ###################
      
      gammaA =  Asym.prop * subpopulation$Asym.prop.mult / d.to.first.sym
      
      gammaS = (1 -  Asym.prop * subpopulation$Asym.prop.mult) / d.to.first.sym
      
      gammaAv =  pc$Asym.prop.vacc * subpopulation$Asym.prop.mult / d.to.first.sym
      
      gammaSv = (1 -  pc$Asym.prop.vacc * subpopulation$Asym.prop.mult) / d.to.first.sym
      
      ###################
      # Presym Flow 
      ###################
      
      E_to_P = nu * E
      
      Ev_to_Pv = nu * Ev
      
      P_to_ISm = gammaS * P
      
      P_to_IA = gammaA * P
      
      Pv_to_ISm = gammaSv * Pv
      
      Pv_to_IAv = gammaAv * Pv
      
      ###################
      # Sym Flow excluding: 
      # Testing and Hospitalization Inflows
      ###################
      
      ISm_to_RS = xi.m * ISm
      
      ISm_to_ISs = upsilon * ISm
      
      ISs_to_RS = xi.s * ISs
      
      ISs_to_D = mu.s * ISs * (1-TreatmentEfficacy)
      
      YSm_to_RS = xi.m.star * YSm
      
      YSm_to_YSs = upsilon.star * YSm
      
      YSs_to_RS = xi.s.star * YSs
      
      # TODO:Raff: Should YSs to D benefit from treatment efficacy?
      YSs_to_D = mu.s * YSs * (1-TreatmentEfficacy)
      
      ###################
      # Hospital OutFlow 
      ###################
      
      H_to_RS = xi.H * H
      
      H_to_D = mu.H * H * (1-TreatmentEfficacy)
      
      ICU_to_RS = xi.ICU * ICU
      
      ICU_to_D = mu.ICU * ICU * (1-TreatmentEfficacy)
      
      ###################
      # Asym Flow 
      ###################
      
      IA_to_RA = xi.A * IA
      
      YA_to_RA = xi.A.star * YA
      
      IAv_to_RAv = xi.A * IAv
      
      YAv_to_RAv = xi.A.star * YAv
      
      ###################
      # Vaccination Out-Flows 
      ###################
      
      Ev_to_Pv =  nu * Ev
      
      Pv_to_IAv = gammaAv * Pv
      
      Pv_to_ISm = gammaSv * Pv
      
      IAv_to_RAv = xi.A * IAv
      
      YAv_to_RAv = xi.A.star * YAv
      
      ###################
      # Sequestration
      ###################
      #dConfined = S_to_C - C_to_S 
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Loss of Immunity
      ###                                                                                        ###
      ##############################################################################################        
      
      
      # Assuming MonthsOfImmunityDurationExists:
      
      # With two stocks:
      RS_to_RB = RS * (1/(pc$MonthsOfImmunityDuration*30)) / 2
      RA_to_RB = RA * (1/(pc$MonthsOfImmunityDuration*30)) / 2
      RAv_to_RB = RAv * (1/(pc$MonthsOfImmunityDuration*30)) / 2
      V_to_RB = V * (1/(pc$MonthsOfImmunityDuration*30)) / 2
      RB_to_S = RB * (1/(pc$MonthsOfImmunityDuration*30)) / 2
      
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Ordinary Differential Equations
      ###                                                                                        ###
      ##############################################################################################
      
      # S_outflows = S_to_E - S_to_V
      
      # S_to_V can never be greater than S - S_to_E:
      
      # if (any(pmin(S_to_V > S - S_to_E + RB_to_S) > 0)){
      #   browser()
      # }
      
      dS =  RB_to_S - S_to_E - S_to_V
      
      dRB = RS_to_RB + RA_to_RB + RAv_to_RB + V_to_RB - RB_to_S
      
      dV = S_to_V - V_to_Ev - V_to_RB
      
      # Incumbation infected and non-infectious stage
      dE = S_to_E - E_to_P
      
      dEv = V_to_Ev - Ev_to_Pv
      
      # Primary stage: incubation infectious stage and asympotomatic
      dP =  E_to_P - P_to_ISm - P_to_IA
      
      dPv =  Ev_to_Pv - Pv_to_ISm - Pv_to_IAv
      
      # Sympotomatic Branch
      dISm = P_to_ISm + Pv_to_ISm - ISm_to_YSm - ISm_to_RS - ISm_to_ISs
      
      dISs = ISm_to_ISs - ISs_to_RS - ISs_to_D - ISs_to_H - ISs_to_YSs
      
      dYSm = ISm_to_YSm - YSm_to_RS - YSm_to_YSs
      
      dYSs = YSm_to_YSs + ISs_to_YSs - YSs_to_RS - YSs_to_H - YSs_to_D
      
      #  Hospitalized
      dH = ISs_to_H + YSs_to_H  - H_to_RS - H_to_D - H_to_ICU
      
      dICU = H_to_ICU - ICU_to_RS - ICU_to_D
      
      # Asympotomatic Branch
      dIA = P_to_IA - IA_to_RA - IA_to_YA
      
      dYA = IA_to_YA - YA_to_RA
      
      dIAv =  Pv_to_IAv - IAv_to_RAv - IAv_to_YAv
      
      dYAv = IAv_to_YAv - YAv_to_RAv
      
      # Final Outcomes
      dD = ISs_to_D + YSs_to_D + H_to_D + ICU_to_D
      
      dRS = ISm_to_RS + ISs_to_RS + YSm_to_RS + YSs_to_RS + H_to_RS + ICU_to_RS - RS_to_RB
      
      dRA =  IA_to_RA + YA_to_RA - RA_to_RB
      
      dRAv = IAv_to_RAv + YAv_to_RAv - RAv_to_RB
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Additional Output States
      ###                                                                                        ###
      ##############################################################################################
      
      dCumulativeRealCases =  S_to_E + V_to_Ev 
      
      dCumulativePositiveTests = ISm_to_YSm + ISs_to_YSs + ISs_to_H + IA_to_YA +  IAv_to_YAv
      
      dCumulativeTotalTests = dCumulativePositiveTests + zeta * (S + E + P + V + Ev + Pv) # TODO: RV revise
      
      dCumulativeReportedRecovered = YA_to_RA + YAv_to_RAv + YSs_to_RS + YSm_to_RS + H_to_RS + ICU_to_RS
      
      dCumulativeReportedDeaths =  YSs_to_D + H_to_D + ICU_to_D + prop.non.hosp.deaths.counted * ISs_to_D
      
      ##############################################################################################
      ###                                                                                        ###
      ###   R effective
      ###                                                                                        ###
      ##############################################################################################
      
      REffective = as.numeric(model$compartment$PopulationShareInLocation %*% cbeta %*% S * tau.eff)
      
      # The Indicator below doesn't work because we don't have enough
      
      # dInfout =  (ISm_to_R + ISs_to_R + YSm_to_R + YSs_to_R +
      #               H_to_R + IA_to_R + YA_to_R + ICU_to_R +
      #               ISs_to_D + H_to_D + ICU_to_D)
      #
      # Infect <- P + ISm + ISs + H + ICU + YSm + YSs + IA + YA
      #
      # ## tau.I and
      #
      # Reff.SEIR <- 1 +
      #   (tau.I + tau.E / 2) * pop_distribution %*% safe_division((E_to_P - dInfout), Infect)
      #
      # Reff.Lipsitch = (tau.I + tau.E) * r.growth + 1 + tau.I * tau.E * (r.growth) ^ 2
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Modeling the Changes in Capacity
      ###                                                                                        ###
      ##############################################################################################
      
      max_testing_capacity = model$capacities$MaxTestingCapacity[model$capacities$LocationID == model$location$LocationID]
      max_ventilator_capacity = model$capacities$MaxVentilatorCapacity[model$capacities$LocationID == model$location$LocationID]
      max_beds_capacity = model$capacities$MaxBedsCapacity[model$capacities$LocationID == model$location$LocationID]
      
      testing_growth_rate = model$capacities$TestingCapacityGrowthRate[model$capacities$LocationID == model$location$LocationID]
      ventilator_growth_rate = model$capacities$VentilatorCapacityGrowthRate[model$capacities$LocationID == model$location$LocationID]
      beds_growth_rate = model$capacities$BedsCapacityGrowthRate[model$capacities$LocationID == model$location$LocationID]
      
      # Smooth Formulation - Intended to work with Lsoda.
      dTestingCapacity = testing_growth_rate * (max_testing_capacity-TestingCapacity)/ (max_testing_capacity * testing.capacity.mult.factor)
      
      dBedsCapacity = beds_growth_rate * (max_beds_capacity-BedsCapacity)/max_beds_capacity
      
      dVentilatorCapacity = ventilator_growth_rate * (max_ventilator_capacity-VentilatorCapacity)/max_ventilator_capacity
      
      dTreatmentEfficacy = as.numeric(date > pc$treatment_improvement_start_date) * (FinalTreatmentEfficacy - TreatmentEfficacy) / (MonthsToMaxTreatmentEfficacy*30)
      
      dNPIStock = (target_npi_portfolio - NPIStock) / DaysToAdjustNPI
      
      
      # print(paste(date, t))
      # # #
      # if(date >= lubridate::as_date("2021-03-13")) {
      #   browser()
      # }
      
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Prepare Flow Outputs
      ###                                                                                        ###
      ##############################################################################################
      
      stocks_vector = c(dRB, dS, dV, dE, dEv, dP, dPv, dISm, dYSm, dISs, dYSs, dH, dICU, 
                        dIA, dIAv, dYA, dYAv, dD, dRS, dRA, dRAv,
                        dCumulativeRealCases, dCumulativePositiveTests, dCumulativeTotalTests,
                        dCumulativeReportedRecovered, dCumulativeReportedDeaths, dCumulativeVaccinated,
                        dTestingCapacity, dBedsCapacity, dVentilatorCapacity, dTreatmentEfficacy, dNPIStock, dVaccineStock)
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Sanity Checks
      ###                                                                                        ###
      ##############################################################################################
      
      # Uncomment for testing:
      #Correct Length
      # if(length(stocks_vector) != length(stocks)) {
      #   browser()
      # }
      # # 
      # # # Any Nan
      # if(any(is.nan(stocks_vector))) {
      #   browser()
      # }
      # 
      # # Any Negative Values
      # if(any(stocks < -1e-7)) {
      #   browser()
      # }
      # 
      # Is it close to 1
      # if(abs(sum(stocks_vector)-1)<1e-10) {
      #   browser()
      # }
      
      #assertthat::assert_that(!any(is.nan(stocks_vector)), "stocks_vector = c(... Error: There are Nan results in your stocks.")
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Return Outputs
      ###                                                                                        ###
      ##############################################################################################
      
      # if(date %in% lubridate::as_date(c("2021-10-20", "2021-01-12"))) {
      #   browser()  
      # }
      
      # Check Slow Runs:
      if(as.numeric(difftime(Sys.time(), pc$initial_time, units = "s")) > timetolerance) {
        
        write.table(file = "slow_runs.txt",append = T,row.names = F, x = data.frame(
          RunID = RunID,
          ExperimentID = ifelse(exists("ExperimentID"),ExperimentID, 0),
          date = date,
          time = as.character(Sys.time()),
          run_id = run_id,
          reopening_criteria_met = reopening_criteria_met,
          stocks_vector = stocks_vector,
          stocks_names = names(stocks)
        )
        )
        
        # Trying first with a stop
        # Testing now change in the model
        #warning("Warning - Model is taking too long to run")
        #return(NULL)
        stop("Model is taking too long to run")
        
      }
      
      return(list(stocks_vector,
                  RB = sum(RB),
                  S = sum(S),
                  Susceptible = sum(S),
                  V = sum(V),
                  E = sum(E),
                  Ev = sum(Ev),
                  P = sum(P),
                  Pv = sum(Pv),
                  ISm = sum(ISm),
                  YSm = sum(YSm),
                  ISs = sum(ISs),
                  YSs = sum(YSm),
                  CurrentlyHospitalized = sum(H+ICU),
                  CurrentlyInICU =  sum(ICU),
                  HospitalCapacity = sum(BedsCapacity),
                  ICUCapacity = sum(VentilatorCapacity),
                  IA = sum(IA),
                  YA = sum(YA),
                  IAv = sum(IAv),
                  YAv = sum(YAv),
                  RS = sum(RS),
                  RA = sum(RA),
                  RAv = sum(RAv),
                  Sumcbeta = sum(cbeta),
                  sumS_to_E = sum(S_to_E),
                  PortfolioID = npi_portfolio, # Return portfolio ID here!
                  Deaths = sum(YSs_to_D + H_to_D + ICU_to_D),
                  S_to_V = sum(S_to_V),
                  Hospitalized = sum(ISs_to_H + YSs_to_H),
                  ICUAdmissions = sum(H_to_ICU),
                  ActualDeaths = sum(ISs_to_D + YSs_to_D + H_to_D + ICU_to_D),
                  PositiveTests = sum(ISm_to_YSm + ISs_to_YSs + ISs_to_H + IA_to_YA + IAv_to_YAv), # The Positive Tests seem wrong
                  NegativeTests = sum(zeta * (S + E + P + V + Ev + Pv) ),
                  TotalTests = sum(dCumulativePositiveTests + zeta * (S + E + P + V + Ev + Pv)),
                  CurrentlyRecovered = sum(RS + RA + RAv + RB),
                  IFR = safe_division(sum(D), (sum(D) + sum(RS + RA + RAv + RB))),
                  CumulativeRealCases = sum(CumulativeRealCases),
                  CumulativePositiveTests = sum(CumulativePositiveTests),
                  CumulativeTotalTests = sum(CumulativeTotalTests),
                  CumulativeReportedRecovered = sum(CumulativeReportedRecovered),
                  CumulativeNegativeTests = sum(CumulativeTotalTests) - sum(CumulativePositiveTests),
                  CumulativeReportedDeaths = sum(CumulativeReportedDeaths),
                  #CumulativeVaccinated = sum(CumulativeVaccinated),
                  #CumulativeFrailVaccinated = sum(CumulativeVaccinated[4:11])/sum(model$compartment$PopulationShareInLocation[4:11]),
                  CumulativeActualDeaths = sum(D),
                  CumulativeDeaths = sum(D),
                  REffective = REffective,
                  seasonal_factor = seasonal_factor,
                  total_vaccines_available = total_vaccines_available,
                  vaccines_unused = vaccines_unused,
                  reopening_criteria_met = reopening_criteria_met,
                  prop_asymp_infections = prop_asymp_infections,
                  # For time profiling purposes:
                  Sys_time = Sys.time(),
                  RunID = run_id))
    })
}

#----------------------------------------------------------------------------------------#
# Function:  Set Stocks Positions
# Purpose: Defines the stocks positions in the vector of stocks.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#

set_stocks_positions.c19model_vamshi4s = function(model) {
  
  # positions = list(
  #   RB = grep("^RB", x = names(model$init_stocks)),
  #   S = grep("^S", x = names(model$init_stocks)),
  #   E = grep("^E", x = names(model$init_stocks)),
  #   Ev = grep("^Ev", x = names(model$init_stocks)),
  #   P = grep("^P", x = names(model$init_stocks)),
  #   Pv = grep("^Pv", x = names(model$init_stocks)),
  #   ISm = grep("^ISm", x = names(model$init_stocks)),
  #   YSm = grep("^YSm", x = names(model$init_stocks)),
  #   ISs = grep("^ISs", x = names(model$init_stocks)),
  #   YSs = grep("^YSs", x = names(model$init_stocks)),
  #   H = grep("^H", x = names(model$init_stocks)),
  #   ICU = grep("^ICU", x = names(model$init_stocks)),
  #   IA = grep("^IA", x = names(model$init_stocks)),
  #   IAv = grep("^IAv", x = names(model$init_stocks)),
  #   YA = grep("^YA", x = names(model$init_stocks)),
  #   YAv = grep("^YAv", x = names(model$init_stocks)),
  #   D = grep("^D", x = names(model$init_stocks)),
  #   RS = grep("^RS", x = names(model$init_stocks)),
  #   RA = grep("^RA", x = names(model$init_stocks)),
  #   RAv = grep("^RAv", x = names(model$init_stocks)),
  #   CumulativeRealCases = grep("^CumulativeRealCases", x = names(model$init_stocks)),
  #   CumulativePositiveTests = grep("^CumulativePositiveTests", x = names(model$init_stocks)),
  #   CumulativeTotalTests = grep("^CumulativeTotalTests", x = names(model$init_stocks)),
  #   CumulativeReportedRecovered = grep("^CumulativeReportedRecovered", x = names(model$init_stocks)),
  #   CumulativeReportedDeaths = grep("^CumulativeReportedDeaths", x = names(model$init_stocks)),
  #   CumulativeVaccinated = grep("^CumulativeVaccinated", x = names(model$init_stocks)),
  #   TestingCapacity = grep("^TestingCapacity", x = names(model$init_stocks)),
  #   BedsCapacity = grep("^BedsCapacity", x = names(model$init_stocks)),
  #   VentilatorCapacity = grep("^VentilatorCapacity", x = names(model$init_stocks))
  # )
  # 
  # model$spos = positions
  
  model
  
}