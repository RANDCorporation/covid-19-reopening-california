

#----------------------------------------------------------------------------------------#
# c19randepimod R package
# This is the R package behind RAND's COVID-19 State policy tool and related papers.
# 
# R Package Author: Pedro Nascimento de Lima
# Model Author: Raffaele Vardavas.
# Copyright (C) 2021 by The RAND Corporation
# See LICENSE.txt and README.txt for information on usage and licensing
#----------------------------------------------------------------------------------------#


#### Inputs Treatment ----------------------------------------------------------------####

#----------------------------------------------------------------------------------------#
# Author: Pedro Nascimento de Lima
# Purpose: This File contains functions that create the inputs object that is used in the
# model.
# The Key object here is the augm_inputs list. This object is created by the
# get_augm_inputs and is modified by all other functions.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#



#' Get Augmented Inputs
#'
#' This function reads an inputs spreadsheet and returns an augmented list of data.frame. The augmented inputs object is used to create models and contain a set of data.frames and objects that represent the initial structure of the model.
#'
#' @param filename standardized inputs xlsx file
#' @param write_to_server if TRUE, writes the augmented inputs file to a server
#' @param read_from_web if TRUE, gets timeseries data from the web, for US states. If you use FALSE, make sure to have a locationtimeseries object in your inputs file.
#' @param start_date The data in which simulation will begin. This data influences the timeseries gathering, and calibration.
#' @param calibration_date Date when the initiation number will be calibrated. You should observe at least one death in each location for that date.
#' @param get_unemployment_data True if user wants to collect and gather unemployment data from pdf data.
#'
#' @return augm_inputs list object, which will be used throughout the package.
#' @export
#' @importFrom magrittr %>%
#' @importFrom stats ave
#' @import dplyr
#' @import tidyr
#' @import tidyr
#' @import readxl
#'
#' @examples
#'\dontrun{
#'augm_inputs = get_augmented_inputs("./data/inputs/input_dataset_combined.xlsx", 
#'                                   start_date = "2020-03-01", 
#'                                  calibration_date = "2020-04-13")
#'}
#'
get_augmented_inputs = function(inputs_path = "./data/inputs", inputs_filename = "input_dataset.xlsx", econ_inputs_filename = "econ_inputs.xlsx", unemployment_data_filename = "unemployment_data.xlsx", write_to_server = F, read_from_web = T, get_countries = F, start_date = "2020-03-01", calibration_date = "2020-04-07", get_unemployment_data = F){
  
  inputs_file = paste0(inputs_path,"/", inputs_filename)  

  if(file.exists(inputs_file)) {
    # augm_inputs =  get_spreadsheet_as_list(filename)
    
    cat_green_tick("Reading Inputs File.")
    
    augm_inputs = get_inputs_object(paste0(inputs_path,"/", inputs_filename))
    
  } else {
    cat_red_bullet(paste0("Input file doesn't exist. Check if ", inputs_file, " exists and is a valid inputs spreadsheet."))
    stop()
  }

  
  ## Substituting Numeric NAs for Predefined Columns:
  subNA_cols = c('NegativeTests', 'PendingTests', 'PositiveTests', 'Deaths')
  # For State Level time series:
  augm_inputs$locationtimeseries[,subNA_cols][is.na(augm_inputs$locationtimeseries[,subNA_cols])] = 0
  # And for Country Level Time Series:
  augm_inputs$countrytimeseries[,subNA_cols][is.na(augm_inputs$countrytimeseries[,subNA_cols])] = 0
  
  # Making sure dates are dates:
  augm_inputs$locationtimeseries$Date = lubridate::as_date(augm_inputs$locationtimeseries$Date)
  augm_inputs$countrytimeseries$Date = lubridate::as_date(augm_inputs$countrytimeseries$Date)
  
  ## Compute Aditional Metrics - For States:
  
  collumns_to_keep = c("LocationID", "Population", "CountryID", "LocationShortName")
  
  collumns_to_drop = names(augm_inputs$location)[!(names(augm_inputs$location) %in% collumns_to_keep)]
  
  # Get data from external APIs / websites.
  
  if(read_from_web) {
    
    unemployment_file = paste0(inputs_path,"/", unemployment_data_filename)  
    
    # Only Get Country Timeseries from the ECDC if asked:
    if(get_countries) {
      augm_inputs$countrytimeseries = get_country_timeseries(augm_inputs, start_date = start_date) 
    }
    
    if(file.exists(unemployment_file)) {
      
      cat_green_tick("Getting data from external APIs.")
      
      location_cases_npi_results = get_location_cases_npi(augm_inputs, start_date = start_date, unemployment_path = unemployment_file, get_unemployment_data = get_unemployment_data)
      
      augm_inputs$timeseriesnas = location_cases_npi_results$NaNs
      
      augm_inputs$locationtimeseries = location_cases_npi_results$Original
      
    } else {
      cat_red_bullet(paste0("Unemployment file doesn't exist. Check if ", unemployment_file, " exists and is a valid inputs spreadsheet."))
      stop()
    }
    
  }
  
  #collumns_to_drop = names(augm_inputs$location)[-1]
  augm_inputs$locationtimeseries = augm_inputs$locationtimeseries %>%
    left_join(augm_inputs$location, by = "LocationID") %>%
    group_by(LocationID) %>%
    arrange(LocationID, Date) %>%
    filter(!is.na(LocationID))
  
  ## Computing a Moving Average By Group:
  augm_inputs$locationtimeseries = transform(augm_inputs$locationtimeseries,
                                             PositiveTestsMovingAverage = ave(PositiveTests, LocationID, FUN = testing_moving_average),
                                             NegativeTestsMovingAverage = ave(NegativeTests, LocationID, FUN = testing_moving_average),
                                             DeathsMovingAverage = ave(Deaths, LocationID, FUN = testing_moving_average),
                                             CurrentlyHospitalizedMovingAverage = ave(CurrentlyHospitalized, LocationID, FUN = testing_moving_average),
                                             HospitalizedMovingAverage = ave(Hospitalized, LocationID, FUN = testing_moving_average),
                                             MaxPortfolioID = ave(PortfolioID, LocationID, FUN = cummax))
  
  augm_inputs$locationtimeseries = augm_inputs$locationtimeseries %>%
    group_by(LocationID) %>%
    arrange(LocationID, Date) %>%
    compute_inputs_aggregated_stats(.) %>%
    select(-one_of(collumns_to_drop))
  
  ## Compute Aditional Metrics - For Countries:
  
  # Keep Only Countries with at least 20 days of data:
  countries_to_keep = augm_inputs$countrytimeseries %>%
    group_by(CountryID) %>%
    count(CountryID) %>%
    filter(n > 20)
  
  augm_inputs$countrytimeseries = augm_inputs$countrytimeseries %>%
    left_join(augm_inputs$country, by = "CountryID") %>%
    group_by(CountryID) %>%
    arrange(CountryID, Date) %>%
    filter(!is.na(CountryID) & CountryID %in% countries_to_keep$CountryID)
  
  augm_inputs$countrytimeseries = transform(augm_inputs$countrytimeseries,
                                            PositiveTestsMovingAverage = ave(PositiveTests, CountryID, FUN = testing_moving_average),
                                            NegativeTestsMovingAverage = ave(NegativeTests, CountryID, FUN = testing_moving_average),
                                            DeathsMovingAverage = ave(Deaths, CountryID, FUN = testing_moving_average),
                                            CurrentlyHospitalizedMovingAverage = ave(CurrentlyHospitalized, CountryID, FUN = testing_moving_average),
                                            HospitalizedMovingAverage = ave(Hospitalized, CountryID, FUN = testing_moving_average),
                                            MaxPortfolioID = ave(x = PortfolioID, CountryID, FUN = cummax))
  
  augm_inputs$countrytimeseries = augm_inputs$countrytimeseries %>%
    group_by(CountryID) %>%
    arrange(CountryID, Date) %>%
    compute_inputs_aggregated_stats(.) %>%
    select(-one_of(collumns_to_drop))
  
  
  # Getting Economic Inputs:
  econ_inputs_file = paste0(inputs_path,"/", econ_inputs_filename)  
  
  if(file.exists(econ_inputs_file)) {
    
    cat_green_tick("Getting economic Inputs")
    
    augm_inputs = augm_inputs %>%
      get_econ_inputs(augm_inputs = ., econ_inputs = econ_inputs_file)
    
  } else {
    cat_red_bullet(paste0("Econ Inputs file doesn't exist. Check if ", econ_inputs_file, " exists and is a valid inputs spreadsheet."))
    stop()
  }
  
  augm_inputs$start_date = lubridate::as_date(start_date)
  
  augm_inputs$calibration_date = lubridate::as_date(calibration_date)
  
  augm_inputs$max_ts_date =  max(augm_inputs$locationtimeseries$Date)
  
  # Then, write to jules if that's necessary:
  if(write_to_server){
    # Saving on Jules and Vincent at the same time:
    
    cat_green_tick("Saving augm_inputs file to server.")
    
    saveRDS(augm_inputs,file = "//vincent/plima/covid-19/app/augm_inputs.rds")
    saveRDS(augm_inputs,file = "//jules/plima/covid-19/app/augm_inputs.rds")
  }
  
  cat_green_tick("Done.")
  
  augm_inputs = augm_inputs %>%
    convert_to_matrices(.) %>%
    set_contact_matrices(.)
  
  check_model_nas(augm_inputs)
  
  augm_inputs
  
}





#----------------------------------------------------------------------------------------#
# Function: Compute Inputs Aggregated Stats
# Purpose: Computes additional statistics from raw data.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#

compute_inputs_aggregated_stats = function(df) {
  df %>%
    # First, compute cumulative numbers, which are also model outputs
    mutate(CumulativeDeaths = cumsum(Deaths),
           CumulativePositiveTests = cumsum(PositiveTests),
           CumulativeNegativeTests = cumsum(NegativeTests),
           CumulativeTotalTests = CumulativePositiveTests + CumulativeNegativeTests,
           TotalDailyTestsMovingAverage = PositiveTestsMovingAverage + NegativeTestsMovingAverage,
           TestingRateMovingAverage = TotalDailyTestsMovingAverage / Population,
           PositivesDailyProportion = PositiveTestsMovingAverage / TotalDailyTestsMovingAverage,
           NegativesDailyProportion = 1 - PositivesDailyProportion,
           PositiveTestsPerBedsMA = PositiveTestsMovingAverage / TotalHospitalBeds,
           CumulativePositiveTestsRatio = CumulativePositiveTests / CumulativeTotalTests,
           HospitalizedPerBeds = CurrentlyHospitalized / TotalHospitalBeds,
           HospitalizedPerICUBeds = CurrentlyHospitalized / TotalICUBeds,
           HospitalizedPerPotentiallyAvailableHospitalBeds = CurrentlyHospitalized / PotentiallyAvailableHospitalBeds,
           HospitalizedPerPotentiallyAvailableICUBeds = CurrentlyHospitalized / PotentiallyAvailableICUBeds) %>%
    compute_agregated_stats(.)
}





#----------------------------------------------------------------------------------------#
# Function: Compute Aggregated Stats
# Purpose: Computes additional statistics from raw data and simulation results.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#


#' computes additional aggregate statistics
#' @export
compute_agregated_stats = function(df) {
  df %>%
    mutate(DeathsPer100K = 100000 * Deaths / Population,
           PositivesPer100K = 100000 * PositiveTests / Population,
           NegativesPer100K = 100000 * NegativeTests / Population) %>%
    mutate(CumulativeDeathsPer100K = 100000 * CumulativeDeaths / Population,
           CumulativePositiveTestsPer100K = 100000 * CumulativePositiveTests / Population,
           CumulativeNegativeTestsPer100K = 100000 * CumulativeNegativeTests / Population,
           CumulativeTotalTestsPer100k = CumulativePositiveTestsPer100K + CumulativeNegativeTestsPer100K,
           CaseFatalityRatio = CumulativeDeaths / CumulativePositiveTests
    )
}





#----------------------------------------------------------------------------------------#
# Functions Created by Lawrence Baker
# Purpose: These functions read data from external APIs for US states.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#



#Lawrence's Functions - Get NPIs and case counts for states on a single sheet

#This function finds the most recent date for an unemployment report
#' @importFrom glue glue
#' @importFrom lubridate now wday days month
get_recent_report_date = function(){
  today <- lubridate::today("EST")
  #checks that the list is out (it comes out in the morning)
  if (lubridate::hour(lubridate::now("EST")) < 9){
    report_date <- today - lubridate::days(1)
  } else {
    report_date <- today
  }
  #if today isn't a Thursday, go backwards until you find a thursday
  while (lubridate::wday(report_date, label=TRUE) != 'Thu'){
    report_date <- report_date - lubridate::days(1)
  }
  return(report_date)
}

#' @importFrom stringr str_replace_all str_remove_all str_squish
extract_employment_data = function(report_date){
  if (lubridate::month(report_date) < 10){
    month_str <-  paste(0, toString(lubridate::month(report_date)), sep="")
  } else {
    month_str <- toString(lubridate::month(report_date))
  }
  if (lubridate::day(report_date) < 10){
    day_str <-  paste(0, toString(lubridate::day(report_date)), sep="")
  } else {
    day_str <- toString(lubridate::day(report_date))
  }
  
  date_str <- paste(month_str, day_str, "20", sep="")
  url <- "https://oui.doleta.gov/press/2020/{date_str}.pdf"
  url <- glue::glue(url)
  
  if(report_date >= lubridate::as_date("2020-10-01")){
    page_num <- 6
  } else {
    page_num <- 5
  }
  
  ## Remove PDF Tools Dependency:
  
  #pdf_file <- pdftools::pdf_subset(url, pages = page_num)
  #pdf_data <- pdftools::pdf_text(pdf_file)
  pdf_data <- stringr::str_split(pdf_data, "\n", simplify = TRUE)
  pdf_data <- pdf_data[4:56]
  pdf_data <- pdf_data[-c(40, 48)]
  initial_claims_date <- report_date - lubridate::days(5)
  insured_unemployment_date <- initial_claims_date - lubridate::days(7)
  data_table <- pdf_data %>%
    stringr::str_replace_all("\\s{2,}", "|")  %>%
    stringr::str_remove_all("\r") %>%
    stringr::str_remove_all("\\*") %>%
    readr::read_delim(delim = "|", col_names =FALSE) %>%
    mutate(Initial_claims_date = initial_claims_date) %>%
    mutate(Insured_unemployment_date = insured_unemployment_date) %>%
    rename(Initial_claims = X2, Insured_unemployment_prev_week = X5, Name = X1) %>%
    select(Name, Initial_claims_date, Initial_claims, Insured_unemployment_date, Insured_unemployment_prev_week) %>%
    as.data.frame()
  data_table$Name <- stringr::str_squish(data_table$Name)
  
  return(data_table)
}

#' @importFrom stringr str_replace_all str_remove_all str_squish str_split
#' @importFrom readr read_delim
extract_employment_data = function(report_date){
  if (lubridate::month(report_date) < 10){
    month_str <-  paste(0, toString(lubridate::month(report_date)), sep="")
  } else {
    month_str <- toString(lubridate::month(report_date))
  }
  if (lubridate::day(report_date) < 10){
    day_str <-  paste(0, toString(lubridate::day(report_date)), sep="")
  } else {
    day_str <- toString(lubridate::day(report_date))
  }
  date_str <- paste(month_str, day_str, "20", sep="")
  
  url <- "https://oui.doleta.gov/press/2020/{date_str}.pdf"
  url <- glue(url)
  
  if(report_date >= lubridate::as_date("2020-10-01")){
    page_num <- 6
  } else {
    page_num <- 5
  }
  
  #pdf_file <- pdftools::pdf_subset(url, pages = page_num)
  #pdf_data <- pdftools::pdf_text(pdf_file)
  pdf_data <- str_split(pdf_data, "\n", simplify = TRUE)
  pdf_data <- pdf_data[4:56]
  pdf_data <- pdf_data[-c(40, 48)]
  initial_claims_date <- report_date - lubridate::days(5)
  insured_unemployment_date <- initial_claims_date - lubridate::days(7)
  data_table <- pdf_data %>%
    stringr::str_replace_all("\\s{2,}", "|")  %>%
    stringr::str_remove_all("\r") %>%
    stringr::str_remove_all("\\*") %>%
    readr::read_delim(delim = "|", col_names =FALSE) %>%
    mutate(Initial_claims_date = initial_claims_date) %>%
    mutate(Insured_unemployment_date = insured_unemployment_date) %>%
    rename(Initial_claims = X2, Insured_unemployment_prev_week = X5, Name = X1) %>%
    select(Name, Initial_claims_date, Initial_claims, Insured_unemployment_date, Insured_unemployment_prev_week) %>%
    as.data.frame()
  data_table$Name <- stringr::str_squish(data_table$Name)
  
  return(data_table)
}

#' @importFrom utils read.csv
get_location_cases_npi = function(inputs, start_date, unemployment_path, get_unemployment_data) {
  
  #Get locations for later reference
  location_lookup = inputs$location
  
  #Get county data from the COVID Tracking project
  state_data_path <- 'https://covidtracking.com/api/v1/states/daily.csv'
  state_df <- read.csv(state_data_path)
  
  #Define a complete datelist and enumerate it for each location
  lnpi_start_date = lubridate::as_date(start_date)
  lnpi_end_date = lubridate::ymd(max(state_df$date))
  dates = lubridate::as_date(lnpi_start_date:lnpi_end_date)
  location_ids = location_lookup$LocationID
  
  time_series <- expand.grid(dates, location_ids)
  names(time_series) = c("Date", "LocationID")
  
  ###Creating the location case data
  #Subset the columns we need
  state_df <- state_df[, c('date', 'state', 'positiveIncrease', 'negativeIncrease', 'hospitalizedIncrease',
                           'hospitalizedCurrently', 'inIcuCurrently', 'inIcuCumulative', 'deathIncrease',
                           'positive', 'negative', 'death', 'hospitalized')]
  
  #Convert date column to date format
  state_df$date = lubridate::ymd(state_df$date)
  
  #Filling in missed cumulative numbers
  state_df <- state_df %>%
    #They miss the first day of increase in their data because of the way they difference
    group_by(state) %>%
    mutate(positiveIncrease = ifelse(date==min(date), positive, positiveIncrease)) %>%
    mutate(negativeIncrease = ifelse(date==min(date), negative, negativeIncrease)) %>%
    mutate(hospitalizedIncrease = ifelse(date==min(date), hospitalized, hospitalizedIncrease)) %>%
    mutate(deathIncrease = ifelse(date==min(date), death, deathIncrease)) %>%
    ungroup() %>%
    #If we cut off the data after it began, need the flows to take account of past cumulative
    mutate(positiveIncrease = ifelse(date==lnpi_start_date, positive, positiveIncrease)) %>%
    mutate(negativeIncrease = ifelse(date==lnpi_start_date, negative, negativeIncrease)) %>%
    mutate(hospitalizedIncrease = ifelse(date==lnpi_start_date, hospitalized, hospitalizedIncrease)) %>%
    mutate(deathIncrease = ifelse(date==lnpi_start_date, death, deathIncrease))
  
  #Get location IDs based on state codes
  state_df <- merge(state_df, location_lookup[, c('LocationShortName', 'LocationID')], by.x='state', by.y='LocationShortName')
  state_df <- state_df %>%
    arrange(LocationID, date) %>%
    group_by(LocationID) %>%
    #Create flags for if data was missing
    mutate(NaN_CurrentlyHospitalized = is.na(hospitalizedCurrently)) %>%
    mutate(NaN_CurrentlyInICU = is.na(inIcuCurrently)) %>%
    mutate(NaN_PositiveTests = is.na(positiveIncrease)) %>%
    mutate(NaN_Deaths = is.na(deathIncrease)) %>%
    #Create ICU flow from cumulative (fill down and then lag)
    fill(inIcuCumulative) %>%
    replace(.,is.na(.), 0) %>%
    mutate(ICU = inIcuCumulative - lag(inIcuCumulative, default = 0)) %>%
    #Rename columns
    rename(Date = date, Hospitalized = hospitalizedIncrease, CurrentlyHospitalized = hospitalizedCurrently, PositiveTests = positiveIncrease,
           NegativeTests = negativeIncrease, Deaths =  deathIncrease, CurrentlyInICU = inIcuCurrently) %>%
    #Define column order
    select(LocationID, Date, PositiveTests, NegativeTests, Hospitalized, CurrentlyHospitalized, ICU,
           CurrentlyInICU, Deaths, NaN_CurrentlyHospitalized, NaN_CurrentlyInICU, NaN_PositiveTests, NaN_Deaths)
  #Add all dates (using timeseries from before)
  state_df <- merge(time_series, state_df, by=c("LocationID","Date"), all.x = TRUE)
  
  #Replace the newly introduced NAs with zeros
  
  state_df <- state_df %>%
    arrange(LocationID, Date) %>%
    mutate(NaN_PositiveTests = ifelse(is.na(PositiveTests), TRUE, NaN_PositiveTests)) %>%
    mutate(NaN_CurrentlyHospitalized = ifelse(is.na(CurrentlyHospitalized), TRUE, NaN_CurrentlyHospitalized)) %>%
    mutate(NaN_CurrentlyInICU = ifelse(is.na(CurrentlyInICU), TRUE, NaN_CurrentlyInICU)) %>%
    mutate(NaN_Deaths = ifelse(is.na(Deaths), TRUE, NaN_Deaths)) %>%
    
    #Replace NA with 0
    replace(.,is.na(.), 0)
  
  ###Now get NPI data
  
  #Get NPI data from google sheet source (managed by BU)
  npi_df <- inputs$npiportfoliotimeseries
  npi_df$Date = lubridate::as_date(npi_df$Date)
  
  #Get mapping of possible scenarios to NPI
  npi_map = inputs$npiportfoliomap
  npi_map <- npi_map %>%
    select(-npi_binary_code)
  
  ### Joining NPIs and cases
  case_npi_df <- npi_df %>%
    right_join(state_df, by=c('LocationID', 'Date')) %>%
    arrange(LocationID, Date) %>%
    group_by(LocationID) %>%
    select(-Name) %>%
    #Fill NPIs downwards
    fill(school_closed:sho_all) %>%
    #Add zeros to empty rows
    replace(.,is.na(.), 0) %>%
    #Based on active NPIs, calcualte the current portfolio
    left_join(npi_map, by=c('school_closed', 'restaurant_closed', 'neb_closed', 'sho_at_risk', 'sho_all')) %>%
    #Drop unneeded columns
    select(-school_closed, -restaurant_closed, -neb_closed, -sho_at_risk, -sho_all)
  
  ###Now we need to get the unemployment data
  
  if(get_unemployment_data) {
    
    #first load in the current data
    unemployment_data <- readxl::read_excel(unemployment_path, sheet= 'unemployment')
    unemployment_data  <- mutate_if(unemployment_data, lubridate::is.POSIXct, lubridate::as_date)
    
    #then check the current report date and the most recent data in data
    final_report_date <- get_recent_report_date()
    final_data_report_date <- lubridate::as_date(max(unemployment_data$UI_report_date))
    
    #if the current report date is after the latest date then we need to get the new data since then
    ui_reports=list()
    if (final_data_report_date <  final_report_date){
      ui_reports[[1]] <- unemployment_data
      i <- 2
      while (final_data_report_date < final_report_date){
        final_data_report_date <- final_data_report_date + lubridate::days(7)
        ui_claims<-extract_employment_data(final_data_report_date)
        ui_claims$UI_report_date <- final_data_report_date
        ui_claims <- merge(ui_claims, location_lookup[, c('Name', 'LocationID')], by='Name')
        if (final_data_report_date == final_report_date){
          date_range = lubridate::as_date(final_data_report_date:lubridate::today("EST"))
        } else {
          day_before_next_report <- final_data_report_date + lubridate::days(6)
          date_range = lubridate::as_date(final_data_report_date:day_before_next_report)
        }
        state_IDs = unique(ui_claims$LocationID)
        time_series <- expand.grid(date_range, state_IDs)
        names(time_series) <- c("Date", "LocationID")
        
        #Join the timeseries with the ui_data
        ui_claims <- ui_claims %>%
          right_join(time_series, by=c('LocationID'))
        #Reorder columns
        ui_reports[[i]] <- ui_claims
        i <- i + 1
      }
      unemployment_data <- dplyr::bind_rows(ui_reports)
    }
    #join with the larger timeseries
    case_npi_df <- case_npi_df %>%
      left_join(unemployment_data, by=c('LocationID', 'Date')) %>%
      #Drop unneeded columns
      select(-Name, -UI_report_date, -Initial_claims_date, -Insured_unemployment_date) %>%
      #copy any numbers down to make sure we don't have NaNs if the report was not released
      fill(Initial_claims, Insured_unemployment_prev_week)
    
    
    # Writing unemployment data to the path:
    #writexl::write_xlsx(x = list(unemployment = unemployment_data), path = unemployment_path)
    
  }
  

  #Create versions with NaN
  case_npi_na_df <- case_npi_df %>%
    mutate(PositiveTests = ifelse(NaN_PositiveTests==1, NA, PositiveTests)) %>%
    mutate(CurrentlyHospitalized = ifelse(NaN_CurrentlyHospitalized==1, NA, CurrentlyHospitalized)) %>%
    mutate(CurrentlyInICU = ifelse(NaN_CurrentlyInICU==1, NA, CurrentlyInICU)) %>%
    mutate(Deaths = ifelse(NaN_Deaths==1, NA, Deaths)) %>%
    select(-NaN_PositiveTests, -NaN_CurrentlyHospitalized, -NaN_CurrentlyInICU, -NaN_Deaths)
  
  case_npi_df <- case_npi_df %>%
    select(-NaN_PositiveTests, -NaN_CurrentlyHospitalized, -NaN_CurrentlyInICU, -NaN_Deaths)
  
  case_npi_timeseries = list(Original = case_npi_df, NaNs = case_npi_na_df)
  
  
  return(case_npi_timeseries)
  
}


#' @importFrom curl curl_download
#' @importFrom readxl read_excel
get_country_timeseries = function(inputs, start_date) {
  
  #Get locations for later reference
  country_lookup = inputs$country
  #Get country data from EU CDC
  country_data_path <- 'https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-07-21.xlsx'
  tf = tempfile(fileext = ".xlsx")
  curl::curl_download(country_data_path, tf)
  country_df <- readxl::read_excel(tf)
  
  
  country_df <- country_df %>%
    select(dateRep, countryterritoryCode, cases, deaths) %>%
    rename(Date = dateRep)
  
  #Convert date column to date format
  country_df$Date = lubridate::ymd(country_df$Date)
  
  #Define a complete datelist and enumerate it for each location
  lnpi_start_date = lubridate::as_date(start_date)
  lnpi_end_date = lubridate::ymd(max(country_df$Date))
  dates = lubridate::as_date(lnpi_start_date:lnpi_end_date)
  country_ids = country_lookup$CountryID
  
  time_series <- expand.grid(dates, country_ids)
  names(time_series) = c("Date", "CountryID")
  
  
  #Get location IDs based on state codes
  country_df <- country_df %>%
    merge(country_lookup[, c('LocationShortName', 'CountryID')], by.x='countryterritoryCode', by.y='LocationShortName') %>%
    arrange(CountryID, Date) %>%
    group_by(CountryID) %>%
    #Rename columns
    rename(LocationShortName = countryterritoryCode, PositiveTests = cases, Deaths =  deaths) %>%
    mutate(NegativeTests=0) %>%
    mutate(Hospitalized=0) %>%
    mutate(CurrentlyHospitalized=0) %>%
    mutate(ICU=0) %>%
    mutate(CurrentlyInICU=0) %>%
    #Define column order
    select(CountryID, Date, PositiveTests, NegativeTests, Hospitalized, CurrentlyHospitalized, ICU,
           CurrentlyInICU, Deaths) %>%
    merge(time_series, by=c("CountryID","Date"), all.y = TRUE) %>%
    replace(.,is.na(.), 0) 
  
  ###Now get NPI data
  
  npi_df <- inputs$npicountrytimeseries
  npi_df$Date = lubridate::as_date(npi_df$Date)
  
  #Get mapping of possible scenarios to NPI
  npi_map = inputs$npiportfoliomap
  npi_map <- npi_map %>%
    select(-npi_binary_code)
  
  
  ### Joining NPIs and cases
  case_npi_df <- npi_df %>%
    right_join(country_df, by=c('CountryID', 'Date')) %>%
    arrange(CountryID, Date) %>%
    group_by(CountryID) %>%
    select(-Name) %>%
    #Fill NPIs downwards
    fill(school_closed:sho_all) %>%
    #Add zeros to empty rows
    replace(.,is.na(.), 0) %>%
    #Based on active NPIs, calcualte the current portfolio
    left_join(npi_map, by=c('school_closed', 'restaurant_closed', 'neb_closed', 'sho_at_risk', 'sho_all')) %>%
    #Drop unneeded columns
    select(-school_closed, -restaurant_closed, -neb_closed, -sho_at_risk, -sho_all) %>%
    #Add in unemployment columns
    mutate(Initial_claims=0) %>%
    mutate(Insured_unemployment_prev_week=0)
  
  return(case_npi_df)
  
}




#----------------------------------------------------------------------------------------#
# Functions Compute Econ History Time Series
# Purpose: Computes Retrospective Economic Outcomes for all cities and States.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#

compute_econ_history_ts = function(inputs) {
  
  inputs$locationtimeseries = inputs$locationtimeseries %>%
    left_join(inputs$npiportfoliolocation %>% filter(EconSensitivity == "B") %>% select(PortfolioID, LocationID, WeeklyGRPLoss, WeeklyIncomeLoss), by = c("LocationID", "PortfolioID")) %>%
    mutate(DailyGRPLoss = WeeklyGRPLoss / 7,
           DailyIncomeLoss = WeeklyIncomeLoss / 7) %>%
    filter(Date >= min(inputs$locationtimeseries$Date))
  
  # inputs$locationtimeseries = inputs$locationtimeseries %>%
  #   left_join(econ_history_ts %>% select(Date, LocationID, PortfolioID, DailyGRPLoss, DailyIncomeLoss), by = c("Date", "LocationID"))
  
  inputs
}



# Function to generate a Settings Object from a spreadsheet
#' Reads the inputs spreadsheet and saves it as a list.
#'
#' This function reads an standardized excel spreadsheet and creates a list containing all the naming conventions that will be used to give variables a user-friendly name. You should only read a standardized data-dictionary.xlsx file. To get one, run create_fam_app_folder() and navigate to the /data folder.
#'
#' @param filename path to data-dictionary.xls file. This file should always be placed inside a /data folder.
#' @import readxl
#'
#' @import dplyr
#' @return a list containing each tab of the data dictionary spreadsheet as a data.frame
#'
get_inputs_object = function(filename = "inputs_data.xlsx") {
  
  sheets <- readxl::excel_sheets(filename)
  
  # First, Read non-Matrices:
  list <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  
  list <- lapply(list, as.data.frame)
  
  names(list) <- sheets
  
  # Cleaning up Duplicated Names in the Compartment Tables:
  drop_cols = c("Location", "SubPopulation")
  
  list$locationcompartment = list$locationcompartment %>% dplyr::select(-one_of(drop_cols))
  list$countrycompartment = list$countrycompartment %>% dplyr::select(-one_of(drop_cols))
  
  # Filter location timeseries with nas - Don't do that while we have collumns with erratic data:
  # list$locationtimeseries = list$locationtimeseries %>% na.omit(.)
  
  list
  
  # This function can be useful if we add IsError Collumns
  # for (sheet in names(x)) {
  #
  #   if("ISError" %in% names(x[[sheet]])) {
  #     x[[sheet]] = x[[sheet]] %>%
  #       select(-(one_of("ISError"))) %>%
  #       # This very special code tells if all rows are NAs
  #       mutate(AllNAs = apply(., 1, function(x) all(is.na(x)))) %>%
  #       filter(AllNAs == F) %>%
  #       select(-AllNAs)
  #   }
  #
  # }
}







#----------------------------------------------------------------------------------------#
# Function: Get Ecnomic Inputs
# Purpose: Reads Economic Inputs from its spreadsheet and appends it to the augm_inputs object.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#

get_econ_inputs = function(econ_inputs, augm_inputs) {
  
  percent_loss = readxl::read_xlsx(path = econ_inputs, sheet = "PercentLoss")
  income_loss = readxl::read_xlsx(path = econ_inputs, sheet = "IncomeLoss")
  
  long_percent_loss = percent_loss %>%
    gather(key = "Name", value = "IncomePercentLoss", - PortfolioID, -EconSensitivity, -Scenario) %>%
    right_join(augm_inputs$location %>% select(LocationID, Name), by = "Name")
  
  long_income_loss = income_loss %>%
    gather(key = "Name", value = "IncomeLoss", - PortfolioID, -EconSensitivity, -Scenario) %>%
    right_join(augm_inputs$location %>% select(LocationID, Name), by = "Name")
  
  econ_inputs = long_percent_loss %>%
    left_join(long_income_loss, by = c("PortfolioID", "EconSensitivity", "Scenario", "Name", "LocationID")) %>%
    mutate(DailyAbsoluteIncomeLoss = IncomeLoss / 7,
           DailyPercentIncomeLoss = IncomePercentLoss / 365) %>%
    select(-Name, -Scenario)
  
  augm_inputs$economicinputs = econ_inputs
  
  augm_inputs
  
}
