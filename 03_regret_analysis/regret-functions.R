
#------------------------------------------------------------------------------#
# Code for the paper: Reopening Under Deep Uncertainty: 
#                     Seeking Robust, Non-Dominated COVID-19 Exit Strategies.
# 
# Author: Pedro Nascimento de Lima
# Copyright (C) 2021 by The RAND Corporation
# See LICENSE.txt and README.txt for information on usage and licensing
#------------------------------------------------------------------------------#

#### REGRET ANALYSIS FUNCTIONS ---------------------------------------------####

#------------------------------------------------------------------------------#
# Author: Pedro Nascimento de Lima
# Purpose: This set of functions are used to prepare the simulation results 
# for an RDM vulnerability analysis using regret measures.
# 
# These functions are specialized for the COVID-19 RDM Analysis paper.
#------------------------------------------------------------------------------#

#' Run Regret Analysis for the COVID-19 model
#'
#' @param experiment_model_rdm model used to run experiments
#' @param results_rdm data.frame with experimental results returned by evaluate experiments
#' @param output_path path for writing outputs
#' @param write_csv TRUE if it should write results to the folder
#'
#' @return a list with two objects. the regret_results contain one row per model run and the corresponding regret measures. The regret_summary contains summary statistics including regret percentiles.
#' @export
#'
run_regret_analysis = function(experiment_model_rdm, results_rdm, output_path = "./output_rdm/", write_csv = T) {

  original_experimental_design = experiment_model_rdm$future_experimental_design
  
  # Creating Strategy codes:
  strategy_codes = original_experimental_design %>%
    mutate(npi_type = ifelse(npi_type == "adaptive-time-based" & NewLevelOfCautionMultiplier == 1, "constant-caution", npi_type)) %>%
    mutate(npi_type_c = recode(npi_type, "adaptive-time-based" = "T", "adaptive-vacc-based" = "V", "constant-caution" = "C")) %>%
    mutate(reopening_criteria_c = recode(reopening_criteria, "All Population" = "A", "Old & Frail" = "O")) %>%
    mutate(vaccination_strategy_name_c = recode(vaccination_strategy_name, "Frailty-Based" = "F", "Mixing-Based-Essential-Workers" = "M")) %>%
    mutate(level_of_caution_c = round(LevelOfCaution, digits = 2)) %>%
    mutate(strategy_group = paste0(npi_type_c, "-", level_of_caution_c)) %>%
    group_by(GridExperimentID, strategy_group) %>%
    summarise(number_of_runs = n(), .groups = "drop") %>%
    ungroup() %>%
    group_by(strategy_group) %>%
    mutate(strategy_sub_number = 1:n()) %>%
    mutate(Strategy = paste0(strategy_group,"-",strategy_sub_number)) %>%
    ungroup() %>%
    select(GridExperimentID, Strategy, strategy_group)
   
  
  # Simplify experimental design table:
  experimental_design = original_experimental_design %>%
    mutate(SOW = paste0(as.character(RunID),as.character(LHSExperimentID))) %>%
    left_join(strategy_codes, by = "GridExperimentID") 

  
  # Create data.frame with strategy descriptions:
  strategy_descriptions = experimental_design %>%
    dplyr::select(Strategy, GridExperimentID, strategy_group, vaccination_strategy_name, npi_type, LevelOfCaution, TransitionDate, NewLevelOfCautionMultiplier, AdaptiveCautionRelaxationRate, AdaptiveCautionMidpoint, reopening_immunity_threshold, reopening_criteria) %>% 
    unique(.) %>%
    mutate(npi_type = recode(npi_type, "adaptive-time-based" = "Time-Based", "adaptive-vacc-based" = "Vaccination-Based"))
  
  
  ## Get Simulation Data, and add Strategy and SOW IDS:
  results_data = results_rdm %>%
    select_last_period(simulation_data = ., time_variable = "time") 
  
  
  # Joining dataset with experimental design
  results_data = results_data %>%
    dplyr::left_join(x = ., y = dplyr::select(.data = experimental_design, ExperimentID, Strategy, SOW), by = "ExperimentID")
  

  # Showing results as numbers / 100 K.
  results_data = results_data %>%
    mutate(across(.cols = contains(c("CumulativeDeaths", "CumulativeRealCases", "YLL")),.fns = ~ .x * 100000)) %>%
    mutate(across(.cols = contains("CumulativePercentIncomeLoss"),.fns = ~ .x * 100))
  
  # Run Regret Analysis for Each outcome of interest:
  death_regret_analysis = compute_and_summarise_regret(data = results_data, outcome_variable = "CumulativeDeaths", sow_variable = "SOW", strategy_variable = "Strategy", regret_type = "min")
  
  income_regret_analysis = compute_and_summarise_regret(data = results_data, outcome_variable = "CumulativePercentIncomeLoss", sow_variable = "SOW", strategy_variable = "Strategy", regret_type = "min")
  
  cases_regret_analysis = compute_and_summarise_regret(data = results_data, outcome_variable = "CumulativeRealCases", sow_variable = "SOW", strategy_variable = "Strategy", regret_type = "min")
  
  yll_regret_analysis = compute_and_summarise_regret(data = results_data, outcome_variable = "YLL", sow_variable = "SOW", strategy_variable = "Strategy", regret_type = "min")
  
  days_regret_analysis = compute_and_summarise_regret(data = results_data, outcome_variable = "days_of_interventions", sow_variable = "SOW", strategy_variable = "Strategy", regret_type = "min")
  
  # Joining Regret Datasets:
  regret_data = death_regret_analysis$data %>%
    left_join(income_regret_analysis$data %>% 
                select(ExperimentID, max_SOW_CumulativePercentIncomeLoss,max_SOW_CumulativePercentIncomeLoss_Strategy,   
                       min_SOW_CumulativePercentIncomeLoss, min_SOW_CumulativePercentIncomeLoss_Strategy, 
                       CumulativePercentIncomeLossRegret,
                       CumulativePercentIncomeLossRegretPerc), by = "ExperimentID") %>%
    left_join(cases_regret_analysis$data %>% 
                select(ExperimentID, max_SOW_CumulativeRealCases, max_SOW_CumulativeRealCases_Strategy,
                       min_SOW_CumulativeRealCases, min_SOW_CumulativeRealCases_Strategy,
                       CumulativeRealCasesRegret,
                       CumulativeRealCasesRegretPerc), by = "ExperimentID") %>%
    left_join(yll_regret_analysis$data %>% 
                select(ExperimentID, max_SOW_YLL, max_SOW_YLL_Strategy, 
                       min_SOW_YLL, min_SOW_YLL_Strategy,
                       YLLRegret,
                       YLLRegretPerc), by = "ExperimentID") %>%
    left_join(days_regret_analysis$data %>% 
                select(ExperimentID, max_SOW_days_of_interventions, max_SOW_days_of_interventions_Strategy,  
                       min_SOW_days_of_interventions, min_SOW_days_of_interventions_Strategy, 
                       days_of_interventionsRegret,
                       days_of_interventionsRegretPerc), by = "ExperimentID")
  
  variables_to_show = c("CumulativePercentIncomeLossRegret", "CumulativeDeathsRegret", "CumulativeRealCasesRegret", "YLLRegret", "days_of_interventionsRegret")
  
  # Joining Strategy Summary Datasets:

  regret_summary_data_wide = death_regret_analysis$StrategySummary %>%
    left_join(income_regret_analysis$StrategySummary, by = "Strategy") %>%
    left_join(cases_regret_analysis$StrategySummary, by = "Strategy") %>%
    left_join(yll_regret_analysis$StrategySummary, by = "Strategy") %>%
    left_join(days_regret_analysis$StrategySummary, by = "Strategy") %>%
    dplyr::select(Strategy, contains("RegretPercentile75")) %>%
    mutate(across(.cols = contains(c("Regret")),.names = "{.col}_normalized", .fns = ~ (.x - min(.x))/(max(.x) - min(.x)))) %>%
    left_join(strategy_descriptions, by = "Strategy") %>%
    mutate(selected = ifelse(CumulativeDeathsRegretPercentile75 < 20, "Yes", "No")) %>%
    arrange(CumulativeDeathsRegretPercentile75) %>%
    mutate(rank_order = row_number()) %>%
    group_by(strategy_group) %>%
    mutate(group_rank = order(order(CumulativeDeathsRegretPercentile75))) %>%
    ungroup(.) %>%
    rename("Deaths" = CumulativeDeathsRegretPercentile75,
           "Income Loss" = CumulativePercentIncomeLossRegretPercentile75, 
           "Cases" = CumulativeRealCasesRegretPercentile75,
           "YLL" = YLLRegretPercentile75,
           "Days of NPIs" = days_of_interventionsRegretPercentile75)
  
  
  ### Pareto Sorting:
  # Perform a pareto sort using the results from the regret analysis.
  # Using only Deaths and Days of NPIs, but other variables could also be used as shown below:
  
  pareto_front = regret_summary_data_wide %>%
    group_by(Strategy) %>%
    # The trick here is to use the group_by dplyr function and refer to the terminal_values$vector directly, which produces a vector.
    # With this approach, each strategy is compared to all other strategies.
    summarise(Dominated = any(
      Deaths > regret_summary_data_wide[,"Deaths"]  &
        # To add a new outcome, add:
        # Outcome > regret_summary_data_wide[,"Outcome"] &
        `Days of NPIs` > regret_summary_data_wide[,"Days of NPIs"]
    )
    ,NotWorst = any(
      Deaths < regret_summary_data_wide[,"Deaths"] & 
        # To add a new outcome, add:
        # Outcome > regret_summary_data_wide[,"Outcome"] &
        `Days of NPIs` < regret_summary_data_wide[,"Days of NPIs"]
    )
    , .groups = "keep") %>%
    mutate(Boundary = !Dominated | !NotWorst)
  
  
  # Adding pareto fronts results to the plot:
  regret_summary_data_wide = regret_summary_data_wide %>%
    left_join(pareto_front, by = "Strategy") %>%
    mutate(Category = ifelse((selected == "Yes") & !Dominated, "Non-Dominated, Selected", "Undetermined")) %>%
    mutate(Category = ifelse((selected == "No") & !Dominated, "Non-Dominated", Category)) %>%
    mutate(Category = ifelse(Dominated, "Dominated", Category))
  
  regret_data = regret_data %>%
    left_join(regret_summary_data_wide %>% dplyr::select(Strategy, rank_order, group_rank, selected, Category), by = "Strategy") %>%
    left_join(strategy_descriptions, by = "Strategy")
  
  
  regret_analysis_results = list(regret_data = regret_data,
                                 regret_summary_data = regret_summary_data_wide,
                                 experimental_design = original_experimental_design
  )
  
  if(write_csv) {
    sapply(names(regret_analysis_results), 
           function (x) write.csv(regret_analysis_results[[x]], file=paste0(output_path,x, ".csv"), row.names = F))
  }
  
  return(regret_analysis_results)
  
}



#' compute_regret
#' 
#' computes regret for an outcome variable using a group variable (usually a variable that represents scenarios, or states of the world)
#'
#' @param data dataframe with simulation results.
#' @param outcome_variable string defining the outcome variable name in the dataset.
#' @param group_variable string defining the grouping variable name
#' @param regret_type either "min" or "max". Use "min" for outcomes to be minimized (e.g. Deaths) or max for outcomes to be maximized (e.g. surplus).
#'
#' @return data.frame including a RegretOutcome Variable.
#' 
#' @export
compute_regret = function(data, outcome_variable, group_variable, strategy_variable, regret_type = "max") {
  max_variable = paste("max", group_variable, outcome_variable, sep = "_")
  min_variable = paste("min", group_variable, outcome_variable, sep = "_")
  regret_variable = paste(outcome_variable, "Regret", sep = "")
  regret_variable_perc = paste(regret_variable, "Perc", sep = "")
  
  # Previously, return only minimum and maximum
  # data[max_variable] = compute_max_by_variable(outcome_variable = outcome_variable, group_variable = group_variable, data = data)
  # 
  # data[min_variable] = compute_min_by_variable(outcome_variable = outcome_variable, group_variable = group_variable, data = data)
  
  # Now, return the full dataframe:
  data = compute_max_by_variable(outcome_variable = outcome_variable, group_variable = group_variable, strategy_variable = strategy_variable, max_variable = max_variable, data = data)
  
  data = compute_min_by_variable(outcome_variable = outcome_variable, group_variable = group_variable, strategy_variable = strategy_variable, min_variable = min_variable, data = data)
  
  if (regret_type == "max") {
    data[regret_variable] = data[max_variable] - data[outcome_variable]  
  } else {
    data[regret_variable] = data[outcome_variable] - data[min_variable]
  }
  
  data[regret_variable_perc] = data[regret_variable] / (data[max_variable] - data[min_variable])
  
  data  
}


##### SUMMARISE OUTCOME VARIABLE #####

#' summarise_outcome_variable
#' 
#' reads a dataset and computes a set of outcomes variables by a grouping variable. for which regret has been computed.
#'
#' @param data dataframe com data para analise do regret.
#' @param outcome_variable variável de resposta para análise do RDM.
#' @param group_variable 
#'
#' @return dataframe com resumo das variaveis por grupo definido.
#' 
#' @export
summarise_outcome_variable = function(data = max_time_data, outcome_variable = "Cash", group_variable = "Strategy") {
  regret_variable = paste(outcome_variable, "Regret", sep = "")
  regret_variable_perc = paste(regret_variable, "Perc", sep = "")
  
  call = substitute(
    expr =
      dplyr::group_by(data, GroupVar) 
    %>% dplyr::select(GroupVar, OutcomeVar, RegretVar, RegretVarPerc)
    %>% dplyr::summarise(
      MeanVar = mean(OutcomeVar, na.rm = TRUE),
      SDVar = sd(OutcomeVar, na.rm = TRUE),
      CVVar = mean(OutcomeVar, na.rm = TRUE) / sd(OutcomeVar, na.rm = TRUE),
      Percentile25Var = quantile(OutcomeVar, probs = c(0.25), na.rm = TRUE),
      Percentile50Var = quantile(OutcomeVar, probs = c(0.5), na.rm = TRUE),
      Percentile75Var = quantile(OutcomeVar, probs = c(0.75), na.rm = TRUE),
      MeanRegret = mean(RegretVar, na.rm = TRUE),
      RegretSD = sd(RegretVar, na.rm = TRUE),
      Regret25Percentile = quantile(RegretVar, probs = c(0.25), na.rm = TRUE),
      Regret50Percentile = quantile(RegretVar, probs = c(0.25), na.rm = TRUE),
      Regret75Percentile = quantile(RegretVar, probs = c(0.75), na.rm = TRUE),
      MeanRegretPerc = mean(RegretVarPerc, na.rm = TRUE),
      RegretSDPerc = sd(RegretVarPerc, na.rm = TRUE),
      Regret25PercentilePerc = quantile(RegretVarPerc, probs = c(0.25), na.rm = TRUE),
      Regret75PercentilePerc = quantile(RegretVarPerc, probs = c(0.75), na.rm = TRUE),
      .groups = "drop_last"
    )
    ,
    env = list(GroupVar = as.name(group_variable),
               OutcomeVar = as.name(outcome_variable),
               RegretVar = as.name(regret_variable),
               RegretVarPerc = as.name(regret_variable_perc)
    )
  )
  
  resumo = eval(call)  
  
  colnames(resumo) = c(
    group_variable,
    paste(outcome_variable, "Mean", sep = ""),
    paste(outcome_variable, "SD", sep = ""),
    paste(outcome_variable, "CV", sep = ""),
    paste(outcome_variable, "Percentile25", sep = ""),
    paste(outcome_variable, "Percentile50", sep = ""),
    paste(outcome_variable, "Percentile75", sep = ""),
    paste(regret_variable, "Mean", sep = ""),
    paste(regret_variable, "SD", sep = ""),
    paste(regret_variable, "Percentile25", sep = ""),
    paste(regret_variable, "Percentile50", sep = ""),
    paste(regret_variable, "Percentile75", sep = ""),
    paste(regret_variable_perc, "Mean", sep = ""),
    paste(regret_variable_perc, "SD", sep = ""),
    paste(regret_variable_perc, "Percentile25", sep = ""),
    paste(regret_variable_perc, "Percentile75", sep = "")
  )
  resumo
}


##### COMPUTE AND SUMMARISE REGRET #####

compute_and_summarise_regret = function(data, outcome_variable, sow_variable, strategy_variable, regret_type = "max") {
  
  if (regret_type == "max") {
    data = compute_regret(data = data, outcome_variable = outcome_variable, group_variable = sow_variable, strategy_variable = strategy_variable, regret_type = "max")  
  } else {
    data = compute_regret(data = data, outcome_variable = outcome_variable, group_variable = sow_variable, strategy_variable = strategy_variable, regret_type = "min")
  }
  
  # Summarising Outcome Variable
  strategy_summary = summarise_outcome_variable(data = data, outcome_variable = outcome_variable, group_variable = strategy_variable)
  
  # Get Output list
  output = list(
    data = data,
    StrategySummary = strategy_summary
  )
  
  output
}

##### CHOOSE STRATEGY #####

choose_strategy_min = function(strategy_summary, criterion) {
  strategy_row_n = which(strategy_summary[criterion] == min(strategy_summary[criterion]))
  strategy = strategy_summary[strategy_row_n, "Strategy"]  
  strategy
}


choose_strategy_max = function(strategy_summary, criterion) {
  strategy_row_n = which(strategy_summary[criterion] == max(strategy_summary[criterion]))
  strategy = strategy_summary[strategy_row_n, "Strategy"]  
  strategy
}




select_last_period = function(simulation_data, time_variable) {
  call = substitute(
    expr = simulation_data %>% dplyr::filter(Time == max(Time)),
    env = list(Time = as.name(time_variable)))
  eval(call)  
}


compute_max_by_variable = function(outcome_variable, group_variable, strategy_variable, max_variable, data) {
  call = substitute(
    expr = {dplyr::group_by(data, GroupVar) %>%
        dplyr::summarise(Maximum = max(OutcomeVar),
                         Strategy = Strategy[which.max(OutcomeVar)],
                         .groups = "drop_last")
    }
    ,
    env = list(GroupVar = as.name(group_variable), OutcomeVar = as.name(outcome_variable)))
  
  max_outcome_variable = eval(call)
  
  max_outcome_variable[max_variable] = max_outcome_variable$Maximum
  
  max_outcome_variable[paste0(max_variable,"_",strategy_variable)] = max_outcome_variable$Strategy
  
  ## Removing Generic Fields:
  max_outcome_variable$Maximum = NULL
  max_outcome_variable$Strategy = NULL
  
  data_join = dplyr::inner_join(data, max_outcome_variable, by = group_variable)
  
  data_join
}

compute_min_by_variable = function(outcome_variable, group_variable, strategy_variable, min_variable, data) {
  call = substitute(
    expr = {dplyr::group_by(data, GroupVar) %>%
        dplyr::summarise(Minimum = min(OutcomeVar),
                         Strategy = Strategy[which.min(OutcomeVar)],
                         .groups = "drop_last")
    }
    ,
    env = list(GroupVar = as.name(group_variable), OutcomeVar = as.name(outcome_variable)))
  
  min_outcome_variable = eval(call)
  
  min_outcome_variable[min_variable] = min_outcome_variable$Minimum
  
  min_outcome_variable[paste0(min_variable,"_",strategy_variable)] = min_outcome_variable$Strategy
  
  ## Removing Generic Fields:
  min_outcome_variable$Minimum = NULL
  min_outcome_variable$Strategy = NULL
  
  data_join = dplyr::inner_join(data, min_outcome_variable, by = group_variable)
  
  data_join
}



### Percentile Functions
# From: https://stackoverflow.com/questions/38775327/ggplot-percentile-lines-by-group-automation
StatPercentileXLabels <- ggproto("StatPercentileXLabels", Stat,
                                 compute_group = function(data, scales, probs) {
                                   percentiles <- quantile(data$x, probs=probs)
                                   data.frame(x=percentiles, y=Inf,
                                              label=paste0("p", probs*100, ": ",
                                                           round(percentiles, digits=3)))
                                 },
                                 required_aes = c("x")
)

stat_percentile_xlab <- function(mapping = NULL, data = NULL, geom = "text",
                                 position = "identity", na.rm = FALSE,
                                 show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = StatPercentileXLabels, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

StatPercentileX <- ggproto("StatPercentileX", Stat,
                           compute_group = function(data, scales, probs) {
                             percentiles <- quantile(data$x, probs=probs)
                             data.frame(xintercept=percentiles)
                           },
                           required_aes = c("x")
)

stat_percentile_x <- function(mapping = NULL, data = NULL, geom = "vline",
                              position = "identity", na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    stat = StatPercentileX, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



parallel_plot = function(regret_summary_data_wide, coordinates, color_variable) {
  # Calculate label positions for each veritcal bar
  lab_x <- rep(1:length(coordinates), times = 2)
  pad_y = 0.1 # 2 times, 1 for min 1 for max
  lab_y <- rep(c(- 0.1, 1.1), each = length(coordinates))
  
  # Get min and max values from original dataset
  lab_z <- c(sapply(regret_summary_data_wide[, coordinates], min), sapply(regret_summary_data_wide[, coordinates], max))
  
  # Convert to character for use as labels
  lab_z <- as.character(round(lab_z))
  
  parallel_plot = regret_summary_data_wide %>%
    dplyr::select(any_of(c(coordinates, color_variable))) %>%
    GGally::ggparcoord(data = ., columns = 1:length(coordinates), groupColumn = color_variable,order = 1:length(coordinates), scale = "uniminmax", showPoints = T) +
    # scale_color_viridis(discrete=TRUE) +
    theme_classic() + 
    theme(legend.position = "bottom") + 
    ylab("75th Regret Percentile") + 
    xlab("") +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_blank()
          ,panel.grid.major.x = element_line(color = "#FFF0F0")
    ) + 
    annotate("text", x = lab_x, y = lab_y, label = lab_z, size = length(coordinates))
  
  parallel_plot
  
}
