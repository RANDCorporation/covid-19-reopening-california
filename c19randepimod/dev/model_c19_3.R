#### COVID-19 Model with AMSH --------------------------------------------------------####

#----------------------------------------------------------------------------------------#
# Authors: Raffaele Vardavas, Pedro Nascimento de Lima
# Purpose: This File contains the c19model3 functions.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#


#' C19 Model 3 Function
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
#'  c19model_amshi3s(., level = "state")
#'}
#'
c19model_amshi3s = function(inputs, level) {
  
  model = inputs %>%
    uniformize_inputs_level(.,level = level) %>%
    set_capacities(.)
  # Initiate Covid-19 model as an empty class.
  # Defining the model geographic level:
  
  model$level = level
  
  model$model_fn = model_function.c19model_amshi3s
  
  model$model_name = "c19model_amshi3s"
  
  model$status = "uncalibrated"
  
  # Uniformize Inputs according to the intended level:
  
  class(model) = c("c19model_amshi3s", "c19model")
  
  model
}

# use this in a near future:
set_calibrated_stocks.c19model_amshi3s = function(model, run_id) {
  
  cal_results = model$cal_results %>%
    filter(RunID == run_id) %>%
    filter(time == max(time))
  
  stocks_list = c(
    Confined = unname(cal_results[,model$spos$Confined+1]),
    S = unname(cal_results[,model$spos$S+1]),
    E = unname(cal_results[,model$spos$E+1]),
    P = unname(cal_results[,model$spos$P+1]),
    ISm = unname(cal_results[,model$spos$ISm+1]),
    YSm = unname(cal_results[,model$spos$YSm+1]),
    ISs = unname(cal_results[,model$spos$ISs+1]),
    YSs = unname(cal_results[,model$spos$YSs+1]),
    H = unname(cal_results[,model$spos$H+1]),
    ICU = unname(cal_results[,model$spos$ICU+1]),
    IA = unname(cal_results[,model$spos$IA+1]),
    YA = unname(cal_results[,model$spos$YA+1]),
    D = unname(cal_results[,model$spos$D+1]),
    R = unname(cal_results[,model$spos$R+1]),
    CumulativeRealCases = unname(cal_results[,model$spos$CumulativeRealCases+1]),
    CumulativePositiveTests = unname(cal_results[,model$spos$CumulativePositiveTests+1]),
    CumulativeTotalTests = unname(cal_results[,model$spos$CumulativeTotalTests+1]),
    CumulativeReportedRecovered = unname(cal_results[,model$spos$CumulativeReportedRecovered+1]),
    CumulativeReportedDeaths = unname(cal_results[,model$spos$CumulativeReportedDeaths+1]),
    TestingCapacity = cal_results$TestingCapacity,
    BedsCapacity = cal_results$BedsCapacity,
    VentilatorCapacity = cal_results$VentilatorCapacity
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

set_initial_stocks.c19model_amshi3s = function(model, initial_exposed) {
  
  # Defining Initial Conditions for Each Stock:
  stocks_list = c(
    Confined = model$compartment$PopulationShareInLocation * 0,
    S = model$compartment$PopulationShareInLocation * (1-initial_exposed),
    E = model$compartment$PopulationShareInLocation * (1/3) * initial_exposed,
    P = model$compartment$PopulationShareInLocation * (1/3) * initial_exposed,
    ISm = model$compartment$PopulationShareInLocation * 0,
    YSm = model$compartment$PopulationShareInLocation * 0,
    ISs = model$compartment$PopulationShareInLocation * 0,
    YSs = model$compartment$PopulationShareInLocation * 0,
    H = model$compartment$PopulationShareInLocation * 0,
    ICU = model$compartment$PopulationShareInLocation * 0,
    IA = model$compartment$PopulationShareInLocation * (1/3) * initial_exposed,
    YA = model$compartment$PopulationShareInLocation * 0,
    D = model$compartment$PopulationShareInLocation * 0,
    R = model$compartment$PopulationShareInLocation * 0,
    CumulativeRealCases = model$compartment$PopulationShareInLocation * 0,
    CumulativePositiveTests = model$compartment$PopulationShareInLocation * 0,
    CumulativeTotalTests = model$compartment$PopulationShareInLocation * 0,
    CumulativeReportedRecovered = model$compartment$PopulationShareInLocation * 0,
    CumulativeReportedDeaths = model$compartment$PopulationShareInLocation * 0,
    TestingCapacity = model$capacities$MinTestingCapacity[model$capacities$LocationID == model$location$LocationID],
    BedsCapacity = model$capacities$MinBedsCapacity[model$capacities$LocationID == model$location$LocationID],
    VentilatorCapacity = model$capacities$MinVentilatorCapacity[model$capacities$LocationID == model$location$LocationID]
  )
  
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

set_computed_params.c19model_amshi3s = function(model) {
  
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
    
    v = Asym.prop / (1 - Asym.prop)
    
    gammaA = Asym.prop / d.to.first.sym
    
    gammaS = (1 - Asym.prop) / d.to.first.sym
    
    ##############################################################################################
    ###                                                                                        ###
    ###   Rates to develop severe symptoms or recover without developing severe sympotoms.
    ###                                                                                        ###
    ##############################################################################################
    
    upsilon = Severe.prop / d.sym.mild
    
    xi = 1 / d.sym.mild - upsilon
    
    ##############################################################################################
    ###                                                                                        ###
    ###   Rates to access the hospital if it is not at Capacity
    ###                                                                                        ###
    ##############################################################################################
    
    h = 1 / d.to.hos
    
    h_y = tested.multi.rate.access.to.hospital * h # faster rate for those tested first.
    
    
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
    
    Die.in.icu.prop = A.icu * Die.in.hos.prop * Frac.deaths.icu /
      (Critical.prop * Frac.deaths.icu +  (1 - Critical.prop) * (1 - Frac.deaths.icu))
    
    mu.icu =  Die.in.icu.prop / d.icu
    
    phi = (1 - Die.in.icu.prop) / d.icu
    
    ##############################################################################################
    ###                                                                                        ###
    ###   Rates to exit the Hospital either due to death (not in ICU) or Recover
    ###                                                                                        ###
    ##############################################################################################
    
    # Rate of Death while hospitalized but not in ICU (including because the ICU is at capacity)
    mu.h  = (1 - A.icu) * chi + (Die.in.hos.prop - A.icu * Critical.prop * Die.in.icu.prop) /
      ((d.hos -  A.icu * Critical.prop * d.icu) * (1 - A.icu *Critical.prop))
    
    xi.h = (1 - Critical.prop) / (d.hos -  A.icu * Critical.prop * d.icu) - mu.h
    
    #chi = A.icu * chi
    
    ##############################################################################################
    ###                                                                                        ###
    ###   Rates of Death and Decovery for those with Severe sympotoms but not hospitalized
    ###                                                                                        ###
    ##############################################################################################
    
    ## Death rates for those with severe sympotoms that do not get hospitalized.
    
    d.to.death.if.not.hos  =  (d.to.hos + d.hos - Critical.prop * d.icu)
    
    #TODO: This assumes that everybody who is critical and does not get hospitalized dies.
    xi.H = (1 - A * Hosp.prop) * (1 - Critical.prop) / d.to.death.if.not.hos
    
    mu.H =  (1 - A * Hosp.prop) * Critical.prop / d.to.death.if.not.hos
    
    ##############################################################################################
    ###   Rate to recovery for those asympotomatic
    ###                                                                                        ###
    ##############################################################################################
    
    pi = 1 / d.asym
    
    ##############################################################################################
    ###                                                                                        ###
    ###   Sequestration
    ###                                                                                        ###
    ##############################################################################################
    
    # d.seq.mixing represents the interval of num of days between any household member leaving the house and
    # significantly mixing. n.nousehold is the number of household members s/he exposes as a result.
    theta = n.household/d.seq.mixing
    
    ##############################################################################################
    ###                                                                                        ###
    ###   R0 and the time scales
    ###                                                                                        ###
    ##############################################################################################
    
    # dummy parameters to express R0.
    
    a = mu.h + xi.h
    
    b = xi + upsilon
    
    c = A * h + mu.H + xi.H
    
    ## Calculate tau to get R0 from growth rate
    
    num1 = a * (gammaA * b * c + pi * (b * c + 1 * gammaS * c + upsilon * 1 * gammaS))
    
    num2 = pi * A * h * upsilon * 1 * gammaS
    
    den = pi * (gammaA + gammaS) * a * b * c
    
    tau.inf = (num1 + num2) / den
    
    tau.exp = 1 / nu
    
    tau = tau.exp + tau.inf
    
    ## Calculate tau.eff to get cbeta
    
    num1 = a * (gammaA * b * c + pi * (b * c + m.Sm * gammaS * c + upsilon * m.Ss * gammaS))
    
    num2 = pi * A * h * upsilon * m.h * gammaS
    
    den = pi * (gammaA + gammaS) * a * b * c
    
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

verify_input_parameters.c19model_amshi3s = function(model) {
  # verify Parametrers
  within(data = as.data.frame(model$scenarios), {
    verify.out <- c(
      "E duration" = 1 / nu,
      "P duration" = 1 / (gammaA + gammaS),
      "Prop asymp" = gammaA / (gammaA + gammaS),
      "Sympotomatic mild duration" = 1 / (upsilon + zetaS + xi),
      "Proportion getting severe" = upsilon / (upsilon + zetaS + xi),
      "Sympotomatic to Hospitalized duration" = 1 / h,
      "Hospital duration" = (1-Critical.prop)*1/(xi.h+mu.h)+A.icu*(Critical.prop)*d.icu,
      "Mortality of non Hosp" =  mu.H / (mu.H + zetaS + xi.H),
      "Mortality of Hosp" =  (1-Critical.prop)*mu.h / (mu.h + xi.h) + Critical.prop*mu.icu / (mu.icu + phi),
      "Mortality of ICU" =  mu.icu / (mu.icu + phi),
      "IA duration" = 1 / pi,
      "IA to tested duration" = 1 / zetaA,
      "generation time scale" = tau,
      "modified time scale for R0" = tau.eff
    )
  })
}


#----------------------------------------------------------------------------------------#
# Function:  Model Function
# Purpose: This function contains the model equations, and represent the actual ODE model.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#
#' @importFrom utils tail
model_function.c19model_amshi3s = function(time, stocks, params, model, run_id) {
  
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
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Unpacking Stocks
      ###                                                                                        ###
      ##############################################################################################
      Confined = stocks[spos$Confined]
      S = stocks[spos$S]
      E = stocks[spos$E]
      P = stocks[spos$P]
      ISm = stocks[spos$ISm]
      YSm = stocks[spos$YSm]
      ISs = stocks[spos$ISs]
      YSs = stocks[spos$YSs]
      H = stocks[spos$H]
      ICU = stocks[spos$ICU]
      IA = stocks[spos$IA]
      YA = stocks[spos$YA]
      D = stocks[spos$D]
      R = stocks[spos$R]
      CumulativeRealCases = stocks[spos$CumulativeRealCases]
      CumulativePositiveTests = stocks[spos$CumulativePositiveTests]
      CumulativeTotalTests = stocks[spos$CumulativeTotalTests]
      CumulativeReportedRecovered = stocks[spos$CumulativeReportedRecovered]
      CumulativeReportedDeaths = stocks[spos$CumulativeReportedDeaths]
      TestingCapacity = stocks[spos$TestingCapacity]
      BedsCapacity = stocks[spos$BedsCapacity]
      VentilatorCapacity = stocks[spos$VentilatorCapacity]
      
      
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
      # Here, cbeta is a matrix dependent on the npi_portfolio, which is fixed for now. K is also fixed but will be calibrated.
      
      # If there is no
      
      date = floor(t) + start_date - 1
      
      #npi_portfolio = 1 # ts$PortfolioID
      npi_portfolio = if(!exists("intervention_schedule") || date <= calibration_date) {
        
        ts$PortfolioID
        
      } else {
        
        # There is an intervention schedule and we are simulating the future:
        tentative_npi = intervention_schedule %>%
          mutate(Passed = date >= ChangeDates) %>%
          filter(Passed) %>%
          filter(ChangeDates == max(ChangeDates))
        
        # If this doesnt return any date, then use the final time series object, otherwise, use the portfolio id coming from the past timeseries.
        ifelse(nrow(tentative_npi) == 1, as.integer(tentative_npi$SimulatedPortfolioID), ts$PortfolioID)
        
      }
      
      
      
      
      # if(npi_portfolio==2) {
      #   browser()
      # }
      
      # if(date >= lubridate::as_date("2020-05-15")) {
      #   browser()
      # }
      
      # npi.cal.factor  = 1 #ifelse(npi_portfolio == 1, 1, npi.efficacy.factor)
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Calculate the c * beta Infectivity and the effective R.
      ###                                                                                        ###
      ##############################################################################################
      
      # Original cbeta formula without calibration factor:
      
      #cbeta = npi.cal.factor * k * model$cbetamatrices[[npi_portfolio]]
      cbeta_no_intervention = k * model$cbetamatrices[[1]]
      
      cbeta = cbeta_no_intervention - (cbeta_no_intervention - k * model$cbetamatrices[[npi_portfolio]]) * npi.cal.factor
      
      cbeta = matrix(pmax(0,cbeta),nrow=nrow(cbeta),ncol=ncol(cbeta))
      
      pop_distribution = model$compartment$PopulationShareInLocation
      
      # Reff = as.numeric(pop_distribution %*% cbeta %*% S * tau.eff)
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Seasonality
      ###                                                                                        ###
      ##############################################################################################
      
      # Below is the function for seasonality - it will be replaced.
      #cbeta <- cbeta * (seas * (1 - (sin(3.14 * t / 365)) ^ 2) + (1 - seas))
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Sequestration: Total Home Confinment
      ###                                                                                        ###
      ##############################################################################################
      
      A.seq = npiportfolio$Sequestration[npiportfolio$PortfolioID == npi_portfolio]
      
      # Old formulation:
      ##eta = theta * exp((n.household - 1) * log(S))
      
      eta =  theta * abs(S) ^ (n.household - 1)
      
      S_to_C = A.seq * eta * S
      
      C_to_S = theta * Confined
      
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Transmission Process
      ###                                                                                        ###
      ##############################################################################################
      
      #### Transmission Process ####
      # S_to_E = cbeta %*% ((P + m.Sm * ISm + m.Ss * ISs +
      #                        m.Sm * YSm +  m.Ss * YSs +
      #                        m.h * (H + ICU) + IA + 1 * YA) * S)
      
      # Original:
      S_to_E = cbeta %*% ((P + m.Sm * ISm + m.Ss * ISs +
                             m.tSm * YSm +  m.tSs * YSs +
                             m.h * (H + ICU) + IA + m.tA * YA) * S)
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Testing Capacity
      ###                                                                                        ###
      ##############################################################################################
      
      # Inputs:
      # MaxBedsCapacity = 0.0001
      # MaximumPossibleTestingRate = 0.005
      # testing_growth_rate = 0.05
      # hosp_cap_growth_rate = 0.0005
      
      #TestingCapacity = min(TestingCapacity, MaximumPossibleTestingRate)
      
      #BedsCapacity = min(BedsCapacity, MaxBedsCapacity)
      
      # First, we assume testing capacity is distributed according to the age distributions:
      InfectedSympNotTestedByStrata = ISs + ISm
      
      test_division_proportion = safe_division(InfectedSympNotTestedByStrata, sum(InfectedSympNotTestedByStrata))
      
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Hospital & ICU Capacity
      ###                                                                                        ###
      ##############################################################################################
      
      ICUCapacityPerStrata = VentilatorCapacity * test_division_proportion
      
      ICUCapacityUtilizationRatio = safe_division(ICU, ICUCapacityPerStrata)
      
      # Using a logistic equn. to get a smooth transition from 1 to 0 and vice-versa for the solver to work.
      A.icu = 1/(1+exp(10*(ICUCapacityUtilizationRatio-1)))
      
      # Turning Capacity Constraint Off:
      A.icu = rep(1, 11)
      
      HospitalCapacityPerStrata = BedsCapacity * test_division_proportion
      
      HospitalCapacityUtilizationRatio = safe_division(H+ICU, HospitalCapacityPerStrata)
      
      # Using a logistic equn. to get a smooth transition from 1 to 0 and vice-versa for the solver to work.
      A = 1/(1+exp(10*(HospitalCapacityUtilizationRatio-1)))
      # Turning Capacity Constraint Off:
      A = rep(1, 11)
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Re-evaluate parameters that depend on Hospital and ICU accessibility
      ###                                                                                        ###
      ##############################################################################################
      
      
      Die.in.icu.prop = A.icu * Die.in.hos.prop * Frac.deaths.icu /
        (Critical.prop * Frac.deaths.icu +  (1 - Critical.prop) * (1 - Frac.deaths.icu))
      
      mu.h  = subpopulation$Die.in.hos.prop.mult * ((1 - A.icu) * chi + (Die.in.hos.prop - A.icu * Critical.prop * Die.in.icu.prop) /
                                                      ((d.hos -  A.icu * Critical.prop * d.icu) * (1 - A.icu * Critical.prop)))
      
      xi.h =  ((1 - Critical.prop) / (d.hos -  A.icu * Critical.prop * d.icu) - mu.h)
      
      mu.H = subpopulation$Die.in.hos.prop.mult * ((1- A * Hosp.prop) * Critical.prop / d.to.death.if.not.hos)
      
      xi.H = ((1 - A * Hosp.prop) * (1 - Critical.prop) / d.to.death.if.not.hos)
      
      mu.icu = mu.icu * subpopulation$Die.in.hos.prop.mult
      
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Model Testing Progression Flows based on Testing Capacity
      ###                                                                                        ###
      ##############################################################################################
      
      #TODO: RV: Need to model those testing of the COVID-Neg & the tested false Negatives of the COVID-Pos
      
      TestingCapacityPerStrata = TestingCapacity * test_division_proportion
      
      # testing time (Two Days:)
      # testing_time = 2
      
      # First, we test people in the hospital:
      # Here, the Hospitalization Capacity is being used:
      ISs_to_H = h * A * ISs
      
      RemainingTests = max(0, sum(TestingCapacityPerStrata - ISs_to_H))
      
      # We could use another way to redistribute tests here:
      RemainingTestsPerStrata = RemainingTests * test_division_proportion
      
      # Then, we test severe cases:
      ISs_to_YSs = pmin((1-A) * ISs * zetaS, RemainingTestsPerStrata)
      
      RemainingTests = sum(RemainingTestsPerStrata - ISs_to_YSs)
      RemainingTestsPerStrata = RemainingTests * test_division_proportion
      
      # Then, we test mild cases:
      #ISm_to_YSm = zetaS * ISm
      ISm_to_YSm = pmin(zetaS * ISm, RemainingTestsPerStrata)
      
      RemainingTests = sum(RemainingTestsPerStrata - ISm_to_YSm)
      RemainingTestsPerStrata = RemainingTests * test_division_proportion
      
      # Then, we test asymptomatic cases:
      #IA_to_YA = zetaA * IA
      IA_to_YA = pmin(zetaA * IA, RemainingTestsPerStrata)
      RemainingTests = sum(RemainingTestsPerStrata - IA_to_YA)
      
      
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Progression Flows
      ###                                                                                        ###
      ##############################################################################################
      gammaA =  Asym.prop * subpopulation$Asym.prop.mult / d.to.first.sym
      
      gammaS = (1 -  Asym.prop * subpopulation$Asym.prop.mult) / d.to.first.sym
      
      
      
      E_to_P = nu * E
      
      P_to_ISm = gammaS * P
      
      P_to_IA = gammaA * P
      
      ISm_to_R = xi * ISm
      
      ISm_to_ISs = upsilon * ISm
      
      ISs_to_R = xi.H * ISs
      
      ISs_to_D = mu.H * ISs
      
      # Here, we apply a maximum hospitalization per strata strategy:
      
      YSs_to_H = h_y * A * YSs
      
      YSm_to_R = xi * YSm
      
      YSm_to_YSs = upsilon * YSm
      
      YSs_to_R = xi.H * YSs
      
      YSs_to_D = mu.H * YSs
      
      H_to_R = xi.h * H
      
      H_to_D = mu.h * H
      
      H_to_ICU = A.icu * chi * H
      
      ICU_to_R = phi * ICU
      
      ICU_to_D = mu.icu * ICU
      
      IA_to_R = pi * IA
      
      YA_to_R = pi * YA
      
      # Sequestration
      dConfined = S_to_C - C_to_S
      
      # Cbeta needs a matrix multiplication operator - Verify this in the future.
      dS = C_to_S - S_to_C - S_to_E
      
      # Incumbation infected and non-infectious stage
      dE = S_to_E - E_to_P
      # Primary stage: incubation infectious stage and asympotomatic
      dP =  E_to_P - P_to_ISm - P_to_IA
      
      # Sympotomatic Branch
      dISm = P_to_ISm - ISm_to_YSm - ISm_to_R - ISm_to_ISs
      
      dISs = ISm_to_ISs - ISs_to_R - ISs_to_D - ISs_to_H - ISs_to_YSs
      
      dYSm = ISm_to_YSm - YSm_to_R - YSm_to_YSs
      
      dYSs = YSm_to_YSs + ISs_to_YSs - YSs_to_R - YSs_to_H - YSs_to_D
      
      #  Hospitalized
      dH = ISs_to_H + YSs_to_H  - H_to_R - H_to_D - H_to_ICU
      
      dICU = H_to_ICU - ICU_to_R - ICU_to_D
      
      # Asympotomatic Branch
      dIA = P_to_IA - IA_to_R - IA_to_YA
      dYA = IA_to_YA - YA_to_R
      
      # Final Outcomes
      dD = ISs_to_D + YSs_to_D + H_to_D + ICU_to_D
      
      dR = ISm_to_R + ISs_to_R + YSm_to_R + YSs_to_R + H_to_R + ICU_to_R + IA_to_R + YA_to_R
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Additional Output States
      ###                                                                                        ###
      ##############################################################################################
      
      # Additional States
      #TODO: Add Sequestration:
      dCumulativeRealCases = -dS
      
      dCumulativePositiveTests = ISm_to_YSm + ISs_to_YSs + ISs_to_H + IA_to_YA
      
      dCumulativeTotalTests = dCumulativePositiveTests + zeta * (S + E + P)
      
      dCumulativeReportedRecovered = YA_to_R + YSs_to_R + YSm_to_R + H_to_R + ICU_to_R
      
      dCumulativeReportedDeaths =  YSs_to_D + H_to_D + ICU_to_D + prop.non.hosp.deaths.counted * ISs_to_D
      
      ##############################################################################################
      ###                                                                                        ###
      ###   R effective
      ###                                                                                        ###
      ##############################################################################################
      
      
      
      REffective = as.numeric(pop_distribution %*% cbeta %*% S * tau.eff)
      
      
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
      
      
      # Linear Growth Formulation:
      # dTestingCapacity = testing_growth_rate * as.numeric(TestingCapacity<max_testing_capacity)
      #
      # dBedsCapacity = beds_growth_rate * as.numeric (BedsCapacity < max_beds_capacity)
      #
      # dVentilatorCapacity = ventilator_growth_rate * as.numeric (VentilatorCapacity < max_ventilator_capacity)
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Prepare Flow Outputs
      ###                                                                                        ###
      ##############################################################################################
      
      stocks_vector = c(dConfined, dS, dE, dP, dISm, dYSm, dISs, dYSs, dH, dICU, dIA, dYA, dD, dR,
                        dCumulativeRealCases, dCumulativePositiveTests, dCumulativeTotalTests,
                        dCumulativeReportedRecovered, dCumulativeReportedDeaths,
                        dTestingCapacity, dBedsCapacity, dVentilatorCapacity)
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Sanity Checks
      ###                                                                                        ###
      ##############################################################################################
      
      if(length(stocks_vector) != length(stocks)) {
        browser()
      }
      if(any(is.nan(stocks_vector))) {
        browser()
      }
      #
      # if(abs(sum(stocks_vector)-1)<1e-10) {
      #   browser()
      # }
      #
      #check_that(!any(is.nan(stocks_vector)), "stocks_vector = c(... Error: There are Nan results in your stocks.")
      
      ##############################################################################################
      ###                                                                                        ###
      ###   Return Outputs
      ###                                                                                        ###
      ##############################################################################################
      
      
      return(list(stocks_vector,
                  Confined = sum(Confined),
                  S = sum(S),
                  E = sum(E),
                  P = sum(P),
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
                  R = sum(R),
                  Sumcbeta = sum(cbeta),
                  sumS_to_E = sum(S_to_E),
                  PortfolioID = npi_portfolio, # Return portfolio ID here!
                  Deaths = sum(YSs_to_D + H_to_D + ICU_to_D),
                  Hospitalized = sum(ISs_to_H + YSs_to_H),
                  ICUAdmissions = sum(H_to_ICU),
                  ActualDeaths = sum(ISs_to_D + YSs_to_D + H_to_D + ICU_to_D),
                  PositiveTests = sum(ISm_to_YSm + ISs_to_YSs + ISs_to_H + IA_to_YA), # The Positive Tests seem wrong
                  NegativeTests = sum(zeta * (S + E + P)),
                  TotalTests = sum(dCumulativePositiveTests + zeta * (S + E + P)),
                  CumulativeRealCases = sum(CumulativeRealCases),
                  CumulativePositiveTests = sum(CumulativePositiveTests),
                  CumulativeTotalTests = sum(CumulativeTotalTests),
                  CumulativeReportedRecovered = sum(CumulativeReportedRecovered),
                  CumulativeNegativeTests = sum(CumulativeTotalTests) - sum(CumulativePositiveTests),
                  CumulativeDeaths = sum(CumulativeReportedDeaths),
                  CumulativeActualDeaths = sum(D),
                  REffective = REffective,
                  RunID = run_id))
    })
}


#----------------------------------------------------------------------------------------#
# Function:  Set Stocks Positions
# Purpose: Defines the stocks positions in the vector of stocks.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#

set_stocks_positions.c19model_amshi3s = function(model) {
  
  positions = list(
    Confined = grep("^Confined", x = names(model$init_stocks)),
    S = grep("^S", x = names(model$init_stocks)),
    E = grep("^E", x = names(model$init_stocks)),
    P = grep("^P", x = names(model$init_stocks)),
    ISm = grep("^ISm", x = names(model$init_stocks)),
    YSm = grep("^YSm", x = names(model$init_stocks)),
    ISs = grep("^ISs", x = names(model$init_stocks)),
    YSs = grep("^YSs", x = names(model$init_stocks)),
    H = grep("^H", x = names(model$init_stocks)),
    ICU = grep("^ICU", x = names(model$init_stocks)),
    IA = grep("^IA", x = names(model$init_stocks)),
    YA = grep("^YA", x = names(model$init_stocks)),
    D = grep("^D", x = names(model$init_stocks)),
    R = grep("^R", x = names(model$init_stocks)),
    CumulativeRealCases = grep("^CumulativeRealCases", x = names(model$init_stocks)),
    CumulativePositiveTests = grep("^CumulativePositiveTests", x = names(model$init_stocks)),
    CumulativeTotalTests = grep("^CumulativeTotalTests", x = names(model$init_stocks)),
    CumulativeReportedRecovered = grep("^CumulativeReportedRecovered", x = names(model$init_stocks)),
    CumulativeReportedDeaths = grep("^CumulativeReportedDeaths", x = names(model$init_stocks)),
    TestingCapacity = grep("^TestingCapacity", x = names(model$init_stocks)),
    BedsCapacity = grep("^BedsCapacity", x = names(model$init_stocks)),
    VentilatorCapacity = grep("^VentilatorCapacity", x = names(model$init_stocks))
  )
  
  model$spos = positions
  
  model
  
}

