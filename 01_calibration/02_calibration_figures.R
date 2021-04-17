

#------------------------------------------------------------------------------#
# Code for the paper: Reopening Under Deep Uncertainty: 
#                     Seeking Robust, Non-Dominated COVID-19 Exit Strategies.
# Copyright (C) 2021 by The RAND Corporation
# See LICENSE.txt and README.txt for information on usage and licensing
#------------------------------------------------------------------------------#

#library(GGally)
#library(ggpubr)
library(tidyr)
library(dplyr)
library(patchwork)


# from https://stackoverflow.com/questions/44961437/how-to-include-density-coloring-in-pairwise-correlation-scatter-plot
my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    stat_density2d(aes(fill=..density..), geom="tile", contour = FALSE) +
    scale_fill_viridis_c()
    #scale_fill_gradientn(colours=rainbow(100))
  p
}



split_camel_case = function(string_vector) {
  split = gsub("([A-Z])", " \\1", string_vector)
  sub("^\\s+", "", split)
}


### Calibration plot - Month Level:
#### ggplots ####
#' @import ggplot2 
#' @importFrom scales trans_breaks trans_format math_format
#' @export
calibration_plot_week = function(model, range = 0.2, max_range_factor = 2.5, s_range = c(0.65,0.9), filter_in_range, time_window_width = 30) {
  
  # Determine date in which first death occurred:
  selecting_start_date = model$timeseries %>%
    #filter(CumulativePositiveTests >= 100) %>%
    ungroup(.) %>%
    mutate(MinDate = min(Date), MaxDate = max(Date))
  
  start_date = selecting_start_date$MinDate[1]
  
  end_date = selecting_start_date$MaxDate[1]
  
  # I Don't need to see every day in the plot
  # model$cali_augm_results = model$cali_augm_results %>%
  #   filter(lubridate::wday(Date) %in% c(1,4))
  
  outcomes = c("CumulativeDeaths", "S")
  
  #outcomes = c("Deaths", "CumulativeDeaths", "S")
  #outcomes = c("CumulativeDeaths","Deaths", "S", "PortfolioID")
  
  # Adding moving average description for those that have moving average
  #
  outcomes_data = outcomes
  #outcomes_data = ifelse(paste0(outcomes, "MovingAverage") %in% names(model$timeseries),paste0(outcomes, "MovingAverage"),outcomes)
  
  # Removing data for those that dont have data:
  outcomes_data = outcomes_data[outcomes_data %in% names(model$timeseries)]
  
  # The calibration_information table is a useful table for verification:
  #writexl::write_xlsx(calibration_information, "calibrationinfo.xlsx")
  
  # Filtering Plot Data:
  timeseries = model$timeseries[,c("LocationID", "Date",outcomes_data)] %>%
    filter(LocationID %in% model$location_ids & Date >= start_date) %>%
    pivot_longer(data = .,cols = all_of(outcomes_data), names_to = "Outcome", values_to = "Value") %>%
    mutate(Date = floor(as.numeric(Date + 1 - min(Date))/time_window_width)+1) %>%
    group_by(Date, LocationID, Outcome) %>%
    summarise(Value = mean(Value), .groups = "drop") %>%
    arrange(LocationID, Outcome, Date) %>%
    mutate(Outcome = split_camel_case(Outcome))
    
  
  # Defining target bounds:
  target_ranges_multipliers = data.frame(Date = 1:length(unique(timeseries$Date)),
                                         multiplier = seq.default(from = max_range_factor, to = 1, length.out = length(unique(timeseries$Date)))
                                         )  
  
  # Computing bounds:
  timeseries_bounds_ranges = timeseries %>%
    filter(Date == max(Date)) %>%
    mutate(variable_range = Value * range) %>%
    dplyr::select(-Date, -Value)
  
  
  timeseries = timeseries %>%
    left_join(timeseries_bounds_ranges, by = c("LocationID", "Outcome")) %>%
    left_join(target_ranges_multipliers, by = "Date") %>%
    ungroup(.) %>%
    mutate(max_bound = ifelse(Outcome %in% c("Cumulative Deaths"), Value + variable_range,NA) ,
           min_bound = ifelse(Outcome %in% c("Cumulative Deaths"), pmax(Value - variable_range*multiplier, 0),NA))
  

  model_data = model$cali_augm_results %>%
    mutate(S = S / Population) %>%
    dplyr::select(all_of(c("LocationID", "Date","RunID",outcomes))) %>%
    filter(Date >= start_date & Date <= end_date) %>%
    #filter(lubridate::wday(Date) %in% c(1,3,5,7)) %>%
    pivot_longer(data = .,cols = all_of(outcomes), names_to = "Outcome", values_to = "Value")%>%
    mutate(Date = floor(as.numeric(Date + 1 - min(Date))/time_window_width)+1) %>%
    group_by(Date, LocationID, Outcome, RunID) %>%
    summarise(Value = mean(Value), .groups = "drop") %>%
    arrange(LocationID, RunID, Outcome, Date) %>%
    mutate(Outcome = split_camel_case(Outcome))
  
  # For Cumulative Deaths:
  timeseries_bounds_selection_deaths = timeseries %>%
    filter(Outcome == "Cumulative Deaths") %>%
    dplyr::select(Date, LocationID, max_bound, min_bound) %>%
    left_join(model_data %>% filter(Outcome == "Cumulative Deaths") %>% dplyr::select(LocationID, Date, Value, RunID), by = c("LocationID", "Date")) %>%
    mutate(within_bounds = Value >= min_bound & Value <= max_bound) %>%
    group_by(RunID) %>%
    summarise(deaths_within_range = all(within_bounds))
  
  # For the number of suscetptibles
  timeseries_bounds_selection_s = model_data %>%
    filter(Outcome == "S" & Date == max(Date)) %>%
    mutate(s_within_range = (Value <= s_range[2]) & (Value >= s_range[1])) %>%
    dplyr::select(RunID, s_within_range)
  
  
  timeseries_bounds_selection = timeseries_bounds_selection_deaths %>%
    left_join(timeseries_bounds_selection_s, by = "RunID") %>%
    mutate(within_range = deaths_within_range & s_within_range)
  
  runs_within_bounds = timeseries_bounds_selection$RunID[timeseries_bounds_selection$within_range]
  
  print(paste0(sum(timeseries_bounds_selection$within_range), " runs within bounds."))
  
  model_data = model_data %>%
    left_join(timeseries_bounds_selection, by = "RunID")
  
  if(filter_in_range){
    model_data = model_data %>%
      filter(within_range)
  }
  
  colors <- c("Model" = "blue", "Data" = "grey50", "Target Bounds" = "grey")
  
  plot = ggplot(model_data, mapping = aes(x=Date, y = Value)) +
    geom_line(mapping = aes(group = RunID,color = "Model"), alpha = 0.5) +
    #geom_vline(xintercept = calibration_information$T0EndDeaths, show.legend = T,linetype="dotted") + 
    geom_line(data = timeseries, mapping = aes(x = Date, y = Value, color = "Data"), size=1) +
    facet_wrap(facets = "Outcome", scales = "free", nrow = 1) + 
    geom_ribbon(data = timeseries, mapping = aes(ymin = min_bound, ymax = max_bound, x = Date, color = "Target Bounds"), alpha = 0.1, show.legend = T) + 
    theme_classic() +
    theme(strip.background = element_blank()) + 
    labs(x = paste0(time_window_width, "-day periods after March 1st, 2020"),
         color = "Legend") +
    scale_color_manual(values = colors) + 
    scale_fill_manual(values = colors)
    
  #guides(color=guide_legend(title="Calibration Method"))
  
  list(plot = plot,
       RunID = runs_within_bounds)

}






# Comparing calibrated models:




# #Using Narrow Bounds
# calibrated_model_imabc_narrow = readRDS(file = "./01_calibration/outputs/calibrated_model_narrow.rds")
# #
# 
# # IMABC constrained:
# calibrated_model_imabc_constrained = readRDS(file = "./01_calibration/outputs/calibrated_model_constrained.rds")
# 
# # IMABC constrained:
# calibrated_model_imabc_constrained_lv4 = readRDS(file = "./01_calibration/outputs/calibrated_model_constrained_lv4.rds")
# 
# # IMABC constrained:
# calibrated_model_imabc_constrained_lv6 = readRDS(file = "./01_calibration/outputs/calibrated_model_constrained_lv6.rds")
# 
# 
# # IMABC constrained:
# calibrated_model_imabc_less_constrained_lv6 = readRDS(file = "./01_calibration/outputs/calibrated_model_less_constrained.rds")
# 
# # Final
# calibrated_model_imabc = readRDS(file = "./01_calibration/outputs/calibrated_model.rds")
# 
# 
# # Using wider bounds
# calibrated_model_imabc_wider = readRDS(file = "./01_calibration/outputs/calibrated_model_wide.rds")
# 
# # Using LHS
# calibrated_model_imabc_old = readRDS(file = "./01_calibration/outputs/calibrated_model_old.rds")
# 
# 
# # Model Previously used:
# calibrated_model_first_analysis = readRDS(file = "./01_calibration/outputs/experiment_model_rdm_first_analysis.rds")
# 
# 
# # Filtering first runs:
# calibrated_model_first_analysis$scenarios = calibrated_model_first_analysis$scenarios %>%
#   filter(RunID %in% calibrated_model_first_analysis$cal_results$RunID)
# 
# 
# # Looking at Rts at the end of calibration:
# mean(calibrated_model_first_analysis$cal_results$REffective[calibrated_model_first_analysis$cal_results$Date == max(calibrated_model_first_analysis$cal_results$Date)])
# 
# 
# # Looking at Rts at the end of calibration:
# mean(calibrated_model_imabc$cal_results$REffective[calibrated_model_imabc$cal_results$Date == max(calibrated_model_imabc$cal_results$Date)])
# 
# # Narrow bounds:
# mean(calibrated_model_imabc_narrow$cal_results$REffective[calibrated_model_imabc_narrow$cal_results$Date == max(calibrated_model_imabc_narrow$cal_results$Date)])
# 
# 
# mean(tail(calibrated_model_first_analysis$cal_results$REffective, n = 1))
# 
# 
# # plotting parameters:
# parameters = calibrated_model_first_analysis$scenarios %>%
#   select(any_of(colnames(calibrated_model_imabc_constrained_lv4$scenarios))) %>%
#   mutate(source = "First Experiment") %>%
#   rbind(.,calibrated_model_imabc_constrained_lv4$scenarios %>%
#           select(any_of(colnames(calibrated_model_first_analysis$scenarios))) %>%
#           mutate(source = "IMABC Const. L4")) %>%
#   rbind(.,calibrated_model_imabc_constrained_lv6$scenarios %>%
#           select(any_of(colnames(calibrated_model_first_analysis$scenarios))) %>%
#           mutate(source = "IMABC Const. L6")) %>%
#   rbind(.,calibrated_model_imabc_less_constrained_lv6$scenarios %>%
#           select(any_of(colnames(calibrated_model_first_analysis$scenarios))) %>%
#           mutate(source = "IMABC Less Ct. L6")) %>%
#   rbind(.,calibrated_model_imabc_wider$scenarios %>%
#           select(any_of(colnames(calibrated_model_first_analysis$scenarios))) %>%
#           mutate(source = "IMABC Wide")) %>%
#   rbind(.,calibrated_model_imabc_constrained$scenarios %>%
#           select(any_of(colnames(calibrated_model_first_analysis$scenarios))) %>%
#           mutate(source = "IMABC Constrained")) %>%
#   rbind(.,calibrated_model_imabc$scenarios %>%
#           select(any_of(colnames(calibrated_model_first_analysis$scenarios))) %>%
#           mutate(source = "IMABC Final")) %>%
#   rbind(.,calibrated_model_imabc_narrow$scenarios %>%
#           select(any_of(colnames(calibrated_model_first_analysis$scenarios))) %>%
#           mutate(source = "IMABC Narrow")) %>%
#   select(where(is.numeric), source)
# 
# 
# 
# max(calibrated_model_first_analysis$cali_augm_results$Date)
# 
# 
# params_long = parameters %>%
#   pivot_longer(cols = -source, names_to = "variable",values_to = "value") %>%
#   group_by(variable, source) %>%
#   summarise(mean = mean(value),
#             max = max(value),
#             min = min(value)) %>%
#   pivot_wider(id_cols = variable)
# 
# 
# # parameters used in calibration:
# calibrated_parameters = calibrated_model_imabc$parameter$InternalName[calibrated_model_imabc$parameter$MaxCalibrationValue!=calibrated_model_imabc$parameter$MinCalibrationValue]
# 
# params_plot_data = parameters %>%
#   #dplyr::select(all_of(calibrated_parameters), source)
#   dplyr::select(npi.cal.factor, behavioral.adaptation.factor,seas,source)
# 
# posterior_plots = GGally::ggpairs(data = params_plot_data, columns = 1:3, mapping = aes(color = source)) +
#   ggpubr::theme_pubclean()
# 
# 
# posterior_plots
# 
# 
# View(calibrated_model_old$parameter)
# 
# ggsave(filename = paste0("./01_calibration/outputs/posterior_surface.pdf"), plot = posterior_plots, device = "pdf", width = 7, height = 4, units = "in", scale = 2)
# 
# 
# 
# 
# 
