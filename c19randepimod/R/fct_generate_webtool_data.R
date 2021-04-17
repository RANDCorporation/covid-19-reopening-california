

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

#' Generate WebTool Data
#'
#' Generates the csv files that are the source of all data at the webtool.
#'
#' @param model epidemiological model to use
#' @param scenario_runs data.frame containing the results from the scenario runs.
#' @param folder folder in which to save csv files
#' @param update_past_data TRUE if you want to update past data
#' @param update_scenarios TRUE if you want to update simulation results
#' @param append TRUE if you want to append results to the csv files
#' 
#' @importFrom magrittr %>%
#' @import dplyr
#' @importFrom utils write.csv write.table
generate_webtool_data = function(model, scenario_runs, folder = "./webdata/", update_past_data = F, update_scenarios = F, append = F) {
  
  if(update_past_data) {
    
    ### Getting NA Indicators:
    nas_indicator = model$timeseriesnas %>%
      mutate(HospNA = is.na(Hospitalized),
             CurrentlyHospNA = is.na(CurrentlyHospitalized),
             ICUNA = is.na(ICU),
             CurrentlyICUNA = is.na(CurrentlyInICU),
             PositivesNA = is.na(PositiveTests),
             DeathsNA = is.na(Deaths)) %>%
      select(LocationID, Date, HospNA, CurrentlyHospNA, ICUNA, CurrentlyICUNA, PositivesNA, DeathsNA)
    
    # Adding indicators to model timeseries:
    model$timeseries = model$timeseries %>%
      left_join(nas_indicator, by = c("LocationID", "Date"))
    
    # Defining the current date:
    ### Locations ###
    locations = model$location # %>% filter(LocationID %in% model$location_ids)
    
    interventions_start = model$timeseries %>%
      filter(PortfolioID != 1) %>%
      filter(Date == min(Date)) %>%
      select(LocationID, Date) %>%
      rename(interventions_start = Date)
    
    cumulative_data = model$timeseries %>%
      mutate(HospitalizedPct = 100000 * Hospitalized / Population,
             ICUPct = 100000 * ICU / Population) %>%
      group_by(LocationID) %>%
      summarise(CumulativeHospitalized = sum(Hospitalized),
                CumulativeICU = sum(ICU),
                CumulativeHospitalizedPct = sum(HospitalizedPct),
                CumulativeICUPct = sum(ICUPct))
    
    complete_locations_data = locations %>%
      left_join(model$timeseries %>% filter(Date == max(Date)) %>% select(-Population), by = "LocationID") %>%
      left_join(model$npiportfolio, by = "PortfolioID") %>%
      left_join(interventions_start, by = "LocationID") %>%
      left_join(cumulative_data, by = "LocationID")
    
    
    final_locations = complete_locations_data %>%
      select(location_id = LocationID,
             location_type = Level,
             location_name = Name,
             intervention_level = InterventionLevel,
             intervention_description = PortfolioDescription,
             testing_status = Testing,
             interventions_start = interventions_start)
    
    final_current_aggregate = complete_locations_data %>%
      mutate(HospitalizationsPct =  ifelse(CurrentlyHospNA, -100, 100000 * CurrentlyHospitalized / Population),
             ICUPct = ifelse(CurrentlyICUNA, -100, 100000 * CurrentlyInICU / Population),
             InitialClaimsPct = 100000 * Initial_claims / Population,
             TotalUnemploymentPct = 100000 * Insured_unemployment_prev_week / Population,
             PositiveTestsPct = ifelse(PositivesNA, -100, 100000 * PositiveTests / Population),
             DeathsPct = ifelse(DeathsNA, -100, 100000 * Deaths / Population),
             Deaths = ifelse(DeathsNA, -100, Deaths),
             CurrentlyHospitalized = ifelse(CurrentlyHospNA, -100, CurrentlyHospitalized),
             CurrentlyInICU = ifelse(CurrentlyICUNA, -100, CurrentlyInICU),
             PositiveTests = ifelse(PositivesNA, -100, PositiveTests)
      ) %>%
      select(location_id = LocationID,
             cases_num = CumulativePositiveTests,
             cases_pct = CumulativePositiveTestsPer100K,
             hospitalizations_num = CurrentlyHospitalized,
             icu_num = CurrentlyInICU,
             population = Population,
             fatalities_num = CumulativeDeaths,
             fatalities_pct = CumulativeDeathsPer100K,
             hospitalizations_pct = HospitalizationsPct,
             icu_pct = ICUPct,
             init_unemployment_num = Initial_claims,
             init_unemployment_pct = InitialClaimsPct,
             total_unemployment_num = Insured_unemployment_prev_week,
             total_unemployment_pct = TotalUnemploymentPct,
             current_cases_num = PositiveTests,
             current_cases_pct = PositiveTestsPct,
             current_fatalities_num = Deaths,
             current_fatalities_pct = DeathsPct,
             cumulative_hospitalizations_num = CumulativeHospitalized,
             cumulative_hospitalizations_pct = CumulativeHospitalizedPct,
             cumulative_icu_num = CumulativeICU,
             cumulative_icu_pct = CumulativeICUPct
      )
    
    
    final_current_timeseries = model$timeseries %>%
      mutate(HospitalizationsPct = ifelse(CurrentlyHospNA, -100, 100000 * CurrentlyHospitalized / Population),
             CurrentlyHospitalized = ifelse(CurrentlyHospNA, -100, CurrentlyHospitalized),
             CurrentlyInICU = ifelse(CurrentlyICUNA, -100, CurrentlyInICU),
             ICUPct =ifelse(CurrentlyICUNA, -100, 100000 * CurrentlyInICU / Population),
             Deaths = ifelse(DeathsNA, -100, Deaths),
             PositiveTests = ifelse(PositivesNA, -100, PositiveTests),
             InitialClaimsPct = 100000 * Initial_claims / Population,
             TotalUnemploymentPct = 100000 * Insured_unemployment_prev_week / Population) %>%
      select(location_id = LocationID,
             date = Date,
             cases_num = CumulativePositiveTests,
             cases_pct = CumulativePositiveTestsPer100K,
             hospitalizations_num = CurrentlyHospitalized,
             icu_num = CurrentlyInICU,
             population = Population,
             fatalities_num = CumulativeDeaths,
             fatalities_pct = CumulativeDeathsPer100K,
             hospitalizations_pct = HospitalizationsPct,
             icu_pct = ICUPct,
             init_unemployment_num = Initial_claims,
             init_unemployment_pct = InitialClaimsPct,
             total_unemployment_num = Insured_unemployment_prev_week,
             total_unemployment_pct = TotalUnemploymentPct,
             current_cases_num = PositiveTests,
             current_fatalities_num = Deaths
      )
    
    
    
    past_data_list = list(
      locations = final_locations,
      current_aggregate = final_current_aggregate,
      current_timeseries = final_current_timeseries
    )
    
    dfs = names(past_data_list)
    
    for(df in dfs){
      file_name = paste0(folder, df, ".csv")
      write.csv(x = past_data_list[[df]], file = file_name, row.names = F)
    }
    
  }
  
  
  if(update_scenarios) {
    
    #
    # # Variables to summarise:
    variables = c("cases", "cumulative_cases", "hospitalizations", "current_hospitalizations", "fatalities", "cumulative_fatalities", "icu", "current_icu")
    
    # Something is wrong here.The lower and the upper bounds seem equal in many cases.
    
    scenario_complete_results = scenario_runs$scenarios_results %>%
      left_join(scenario_runs$future_runs, by = c("RunID", "LocationID", "FutureScenarioID", "FutureScenarioRepID")) %>%
      left_join(model$npiportfolio %>% select(PortfolioID, InterventionLevel), by = c("Portfolio1ID" = "PortfolioID"))
    # To make the file smaller, we could report every day for the first month and then every 4 days going forward.
    # We could do that here.
    
    # This full Join gets simulated data from the past:
    past_scenarios_complete_results = calibrated_model$cali_augm_results %>%
      filter(RunID %in% unique(scenario_runs$future_runs$RunID)) %>%
      full_join(scenario_runs$future_runs, by = c("RunID", "LocationID")) %>%
      left_join(model$npiportfolio %>% select(PortfolioID, InterventionLevel), by = c("Portfolio1ID" = "PortfolioID"))
    
    
    # Check if both dataframes contain the same variables:
    if(!dplyr::setequal(names(past_scenarios_complete_results), names(scenario_complete_results))) {
      # Names are not equal
      cat_green_tick("The variables on calibration model results set are not the same as the variables in the scenario runs. Consider computing economic outputs during the calibration process as well.")
      
      names(past_scenarios_complete_results)
      
      names(scenario_complete_results)
      
      check_that(dplyr::setequal(names(past_scenarios_complete_results), names(scenario_complete_results)))
    }
    
    final_complete_results = rbind(past_scenarios_complete_results, scenario_complete_results) %>%
      arrange(LocationID, FutureScenarioRepID, Date)
    
    intervention_dates = unique(scenario_runs$future_runs[,c("Portfolio1StartDateID", "Portfolio1StartDate")])
    names(intervention_dates) = c("intervention_date_id", "intervention_date")
    
    
    final_projected_time_series = final_complete_results %>%
      rename(location_id = LocationID,
             future_scenario_id = FutureScenarioID,
             cases = PositiveTests,
             cumulative_cases = CumulativePositiveTests,
             hospitalizations = Hospitalized,
             current_hospitalizations = CurrentlyHospitalized,
             fatalities = Deaths,
             cumulative_fatalities = CumulativeDeaths,
             date = Date,
             portfolio_id = Portfolio1ID,
             intervention_level = InterventionLevel,
             icu = ICUAdmissions,
             current_icu = CurrentlyInICU,
             intervention_length = Portfolio1Duration,
             intervention_date_id = Portfolio1StartDateID
      ) %>%
      group_by(location_id, portfolio_id, future_scenario_id, date, intervention_level, intervention_length, intervention_date_id) %>%
      summarise_at(.vars = variables, .funs = list(num = ~mean(.),
                                                   lower_bound = ~min(.),
                                                   upper_bound = ~max(.))) %>%
      mutate_if(is.numeric, ~pmax(.,0)) %>%
      mutate_if(is.numeric,~round(.,digits = 2))
    
    final_projected_aggregate = final_complete_results %>%
      group_by(LocationID, Portfolio1ID, InterventionLevel, Portfolio1Duration, Portfolio1StartDateID, FutureScenarioID, Date) %>%
      # First, we have to aggregate at the Future Scenario ID, and we will take the average of all runs, at each date:
      summarise(gsp_num = mean(DailyAbsoluteIncomeLoss),
                gsp_pct = mean(DailyPercentIncomeLoss),
                hh_income_num = 0.0,
                hh_income_pct = 0.0) %>%
      # Now we know that we are seeing only one result per scenario and we can sum:
      group_by(LocationID, Portfolio1ID, InterventionLevel, Portfolio1Duration, Portfolio1StartDateID) %>%
      summarise(gsp_num = sum(gsp_num),
                gsp_pct = sum(gsp_pct),
                hh_income_num = 0.0,
                hh_income_pct = 0.0) %>%
      rename(location_id = LocationID, portfolio_id = Portfolio1ID, intervention_level = InterventionLevel, intervention_length = Portfolio1Duration, intervention_date_id = Portfolio1StartDateID)
    
    
    #
    # Building final list
    
    projected_data_list = list(
      projected_time_series = final_projected_time_series,
      projected_aggregate = final_projected_aggregate
    )
    
    # Saving calibrated model
    #saveRDS(model, paste0(folder,"calibrated_model.rds"))
    
    # Writing Data:
    dfs = names(projected_data_list)
    
    write.csv(intervention_dates, file = paste0(folder, "intervention_dates.csv"), row.names = F)
    
    for(df in dfs){
      file_name = paste0(folder, df, ".csv")
      write.table(x = projected_data_list[[df]],sep = ",", col.names = !file.exists(file_name), file = file_name, row.names = F, append = append)
    }
    
  }
  
  cat_green_tick("Finishing Webtool Data Generation")
  
  Sys.time()
  
}
