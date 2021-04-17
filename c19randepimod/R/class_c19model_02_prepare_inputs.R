

#----------------------------------------------------------------------------------------#
# c19randepimod R package
# This is the R package behind RAND's COVID-19 State policy tool and related papers.
# 
# R Package Author: Pedro Nascimento de Lima
# Model Author: Raffaele Vardavas.
# Copyright (C) 2021 by The RAND Corporation
# See LICENSE.txt and README.txt for information on usage and licensing
#----------------------------------------------------------------------------------------#



#### Prepare Inputs Functions---------------------------------------------------------####

#----------------------------------------------------------------------------------------#
# Author: Pedro Nascimento de Lima
# Purpose: This File contains functions that take the inputs object and prepare it
# to be used by the simulation model.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#


#' Prepare Inputs
#'
#' Prepares inputs for the simulation model. This includes converting tables to matrices, computing contact matrices, defining calibration dates and running exponential regressions.
#'
#' @param augm_inputs inputs object created with the get_augmented_inputs function.
#'
#' @return The transformed inputs object
#' @export
#' @examples
#'\dontrun{
#'model_inputs = augm_inputs %>% prepare_inputs()
#'}
#'
prepare_inputs = function(augm_inputs) {
  
  # Read Augmented Inputs and saves an input object that is appropriate for calibration
  # Prepare inputs will no longer be used.
  
  prepared_inputs = augm_inputs # %>%
    #convert_to_matrices(.) %>%
    #set_contact_matrices(.) %>%
    # Moving set_calibration_dates and run_exponential_regressions to the model function.
    #set_calibration_dates(.) %>%
    #run_exponential_regressions(.) # %>%
  
  #complete_npilocationtimeseries(.)
  
  # Return List of prepared Inputs:
  prepared_inputs
}


#----------------------------------------------------------------------------------------#
# Function: Convert To Matrices
# Purpose: Converts dataframes that start with m_ to matrices:
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#

convert_to_matrices = function(list) {
  
  dataframe_names = names(list)
  
  # Matrices:
  matrices_names = dataframe_names[grepl(pattern = "m_", dataframe_names)]
  
  # Non-Matrices:
  non_matrices_names = dataframe_names[!(dataframe_names %in% matrices_names)]
  
  # First, Read non-Matrices:
  non_matrices <- list[non_matrices_names]
  
  # Reading Matrices
  matrices_dfs = list[matrices_names]
  
  # Turning matrices into actual matrix format:
  matrices = lapply(matrices_dfs, function(df){
    matrix = as.matrix(df[,2:ncol(df)])
    rownames(matrix) = as.vector(df[,1])
    matrix
  })
  
  names(matrices) <-  matrices_names
  
  c(non_matrices, matrices)
  
}




#----------------------------------------------------------------------------------------#
# Function: Set Contact Matrices
# Purpose: Computes Contact Matrices for Each NPI Portfolio.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#

set_contact_matrices = function(inputs) {
  
  # Setting Up local variables:
  modes_names <- inputs$contactmodes$ModeName
  
  modes_ids = inputs$contactmodes$ModeID
  
  kappa <- inputs$pop.prop
  
  strata_names <- inputs$subpopulation$SubPopulationName
  
  strata_ids = inputs$subpopulation$SubPopulationID
  
  # Starting With a Blank list of matrices:
  
  cbeta = list()
  
  # For Each NPI Portfolio:
  for (npi in unique(inputs$npiportfoliovscontactweights$PortfolioID)) {
    
    w.sq.df = inputs$npiportfoliovscontactweights %>% filter(PortfolioID == npi)
    
    #w.sq <- mix.matrix$w
    
    #M.status.quo<- 0
    contact_matrix = 0
    
    for (m in modes_ids){
      
      #x <- kappa[m][,1]
      x = kappa[[modes_names[m]]]
      
      x_diag = diag(x, length(x))
      
      #x<- diag(x , length(x) )
      
      colnames(x_diag) <- strata_names
      rownames(x_diag) <- strata_names
      
      portfolio_vectors = inputs$npiportfoliovectors %>% 
        filter(PortfolioID == npi, ModeName == modes_names[m])
      
      mixing_modifier = sqrt(diag(portfolio_vectors$MixingModifier))  
      
      # y before using the portfolio vectors:
      y <- x_diag %*% inputs[[paste0("m_", modes_names[m])]] 
      
      mode_weight = w.sq.df$Weights[w.sq.df$ContactModeID == m]
      
      contact_matrix <- contact_matrix +  mode_weight * mixing_modifier %*% y %*% mixing_modifier
      
    }
    
    cbeta[[npi]] = contact_matrix
    
  }
  
  # And Each Mode:
  
  # Appending these matrices to the input object:
  inputs$cbetamatrices = cbeta
  
  # And returning the inputs object:
  
  inputs
  
}


#----------------------------------------------------------------------------------------#
# Function: Set Calibration Dates
# Purpose:  This function uses the npi timeseries to set the dates in which the model should stop using case count and deaths data to calibrate the gradient of the epidemic growth.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#

set_calibration_dates = function(inputs, death_lag = 14, cases_lag = 7) {
  
  computed_t0 = inputs$timeseries %>%
    filter(PortfolioID == 1) %>%
    group_by(LocationID) %>%
    summarise(LastDateWithnoIntervention = max(Date)) %>%
    mutate(T0EndDeaths = LastDateWithnoIntervention + death_lag,
           T0EndCases = LastDateWithnoIntervention + cases_lag)
  
  inputs$calibration = inputs$calibration %>%
    left_join(computed_t0, by = "LocationID")
  
  inputs
  
}





#----------------------------------------------------------------------------------------#
# Function: Run Exponential Regressions
# Purpose:  This function Runs exponential regressions for locations and obtains the gradient of the disease invasion phase.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#
#' @importFrom stats lm
run_exponential_regressions = function(augm_inputs, min_cases = 20, min_deaths = 1,  min_cumulative_cases = 50, min_cumulative_deaths = 5) {
  
  location_data = augm_inputs$timeseries %>%
    filter(!is.na(LocationID) & CumulativePositiveTests >= min_cases & CumulativeDeaths >= min_deaths) %>%
    arrange(LocationID, Date) %>%
    select(LocationID, Date, CumulativePositiveTests, CumulativeDeaths) %>%
    mutate(time = row_number(),
           YCases = log(CumulativePositiveTests),
           YDeaths = log(CumulativeDeaths)) %>%
    left_join(augm_inputs$calibration, by = "LocationID")
  
  # For now, we have to disable countries until we have country-level data. In this case, this means defining a T0EndCases and a T0EndDeaths.
  #T0EndCases and T0EndDeaths mean the dates in which we should stop using data to calibrate the gradient of the growth of the epidemic.
  
  # country_data = augm_inputs$countrytimeseries %>%
  #   filter(CumulativePositiveTests >= min_cases & CumulativeDeaths >= min_deaths & !is.na(CountryID)) %>%
  #   arrange(CountryID, Date) %>%
  #   select(CountryID, Date, CumulativePositiveTests, CumulativeDeaths) %>%
  #   left_join(augm_inputs$countrycalibration, by = "CountryID") %>%
  #   rename(LocationID = CountryID) %>%
  #   mutate(time = row_number(),
  #          YCases = log(CumulativePositiveTests),
  #          YDeaths = log(CumulativeDeaths))
  #
  # country_data_t0 = country_data %>%
  #   filter(Date <= T0End)
  #
  # country_data_t1 = country_data %>%
  #   filter(T0End < Date & Date <= T1End)
  #
  # This local function runs a log regression that extracts the gradient of the epidemic growth during the exponential phase:
  run_regression = function(id, location_data) {
    
    # Filtering for the specific id:
    location_data = location_data %>% filter(LocationID == id)
    
    # Trying to run Regressions on Deaths:
    
    # First, filter location data to include only dates in which we can calibrate:
    location_data_cases = location_data %>%
      filter(CumulativePositiveTests >= min_cases & Date <= T0EndCases)
    
    location_data_deaths = location_data %>%
      filter(CumulativeDeaths >= min_deaths & Date <= T0EndDeaths)
    
    # Only run regressions if we have at least 10 days of data:
    if(nrow(location_data_cases) > 1 & nrow(location_data_deaths) > 1) {
      
      # Only regress deaths if there are enough observed deaths:
      if(max(location_data_deaths$CumulativeDeaths) >= min_cumulative_deaths & nrow(location_data_deaths) >= 5) {
        
        # There is enough data for a regression with cumulative deaths
        # Do Cumulative Deaths Regression
        reg_deaths = lm(YDeaths ~ time, data = location_data_deaths)
        
        summary_deaths = summary(reg_deaths)
        deaths_r_squared = summary_deaths$r.squared
        deaths_se = 1/summary_deaths$coefficients[4]
        deaths_coeff = reg_deaths$coefficients["time"]
        
        # If we used deaths, we don't want to use cases:
        # set rsquared to zero
        cases_r_squared = 0
        cases_coeff = 0
        
      } else {
        
        # Don't use deaths regression
        # set rsquared to zero
        deaths_r_squared = 0
        deaths_coeff = 0
        
        # We Only calibrate using cases if calibrating for deaths deos not work:
        
        # Only regress Cases if there are enough observed Cases:
        if(max(location_data_cases$CumulativePositiveTests) >= min_cumulative_cases & nrow(location_data_cases) >= 5) {
          
          # There is enough data for a regression with cumulative deaths
          # Do Cumulative Deaths Regression
          reg_cases = lm(YCases ~ time, data = location_data_cases)
          
          summary_cases = summary(reg_cases)
          cases_r_squared = summary_cases$r.squared
          cases_se = 1/summary_cases$coefficients[4]
          cases_coeff = reg_cases$coefficients["time"]
          
        } else {
          # Don't use deaths regression
          # set rsquared to zero
          cases_r_squared = 0
          cases_coeff = 0
        }
        
      }
      
      # Here we have either the Cases regression or the deaths regression:
      MaxT0CoeffRSquared = max(cases_r_squared, deaths_r_squared)
      
      if((deaths_r_squared + cases_r_squared)>0) {
        
        # First iteration - weighted average:
        #weight_cases <- cases_r_squared / (deaths_r_squared + cases_r_squared)
        #T0Coeff = weight_cases * cases_coeff + (1-weight_cases) * deaths_coeff
        
        # Second iteration - using the maximum gradient:
        T0Coeff = max(cases_coeff, deaths_coeff)
        
      } else {
        MaxT0CoeffRSquared = 0
        weight_cases = NA
        T0Coeff = 0
      }
      # Don't do a regression and set the max r to zero for both deaths and cases:
    } else {
      
      # There's not enough data to compute a gradient for this based on data.
      MaxT0CoeffRSquared = 0
      T0Coeff = 0
      cases_coeff = 0
      cases_r_squared = 0
      deaths_coeff = 0
      deaths_r_squared = 0
      
    }
    
    # Returing everything as a data.frame:
    data.frame(LocationID = id,
               T0Coeff = T0Coeff,
               MaxT0CoeffRSquared = MaxT0CoeffRSquared,
               cases_coeff = cases_coeff,
               cases_r_squared = cases_r_squared,
               deaths_coeff = deaths_coeff,
               deaths_r_squared = deaths_r_squared)
    
  }
  
  # And this function wraps the above function and returns a data.frame with the coefficients:
  get_coefficients_for_t = function(location_data, location_ids) {
    locations_with_data = unique(location_data$LocationID)
    
    location_coefficients = do.call(rbind, lapply(locations_with_data, run_regression, location_data))
    
    # location_coefficients = data.frame(LocationID = locations_with_data,
    #                                   Coefficient = sapply(locations_with_data, run_regression, location_data = location_data))
    
    # names(location_coefficients) = c("LocationID", t_pref)
    
    data.frame(LocationID = location_ids) %>%
      left_join(location_coefficients, by = "LocationID") %>%
      mutate_each(~replace(., which(is.na(.)), 0))
    
  }
  
  # So they now can be used to get all coefficients:
  #t0_country = get_coefficients_for_t(country_data_t0, augm_inputs$country$CountryID)
  #t1_country = get_coefficients_for_t(country_data_t1, "T1Coeff")
  
  t0_location = get_coefficients_for_t(location_data, augm_inputs$location$LocationID)
  #t1_location = get_coefficients_for_t(location_data_t1, "T1Coeff")
  
  # Saving those numbers in the country and location table:
  # augm_inputs$country = augm_inputs$country %>%
  #   left_join(t0_country, by = c("CountryID" = "LocationID")) # %>%
  #left_join(t1_country, by = c("CountryID" = "LocationID"))
  
  augm_inputs$location = augm_inputs$location %>%
    left_join(t0_location, by = "LocationID") # %>%
  #left_join(t1_location, by = "LocationID")
  
  augm_inputs
  
}
