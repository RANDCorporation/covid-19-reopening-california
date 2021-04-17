

#----------------------------------------------------------------------------------------#
# c19randepimod R package
# This is the R package behind RAND's COVID-19 State policy tool and related papers.
# 
# R Package Author: Pedro Nascimento de Lima
# Model Author: Raffaele Vardavas.
# Copyright (C) 2021 by The RAND Corporation
# See LICENSE.txt and README.txt for information on usage and licensing
#----------------------------------------------------------------------------------------#


#### Plots (For the Shiny App)--------------------------------------------------------####

#----------------------------------------------------------------------------------------#
# Author: Pedro Nascimento de Lima
# Purpose: This File contains functions that create plots.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#



#### HCPlots ####

# @import highcharter 
# @export
# model_fit_plot_hc = function(model, location_short_name = "NY", outcome_variable = "CumulativePositiveTests", log_scale = T) {
#   
#   # This data-wrangling part should probably be generalized:
#   
#   location_id = model$location$LocationID[model$location$LocationShortName %in% location_short_name]
#   
#   min_date_ts = model$timeseries %>%
#     filter(CumulativePositiveTests >= 50)
#   
#   min_date = min(min_date_ts$Date)
#   
#   max_date = max(model$timeseries$Date)
#   
#   timeseries_data = model$timeseries %>%
#     filter(LocationID %in% location_id & CumulativePositiveTests > 50 & Date <= max_date & Date >= min_date)
#   
#   model_data = model$cali_augm_results %>%
#     filter(LocationID %in% location_id & CumulativePositiveTests > 50 & Date <= max_date & Date >= min_date)
#   
#   
#   timeseries_data$PlotOutcome = timeseries_data[[outcome_variable]]
#   model_data$PlotOutcome = round(model_data[[outcome_variable]], digits = 2)
#   
#   
#   if(log_scale){
#     timeseries_data$PlotOutcome = round(log10(timeseries_data$PlotOutcome), 2)
#     model_data$PlotOutcome = round(log10(model_data$PlotOutcome), 2)
#   }
#   
#   highchart() %>%
#     # The linke below format the dates:
#     #hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>%
#     hc_xAxis(type = "datetime") %>%
#     hc_yAxis(title = list(text = paste0(split_camel_case(outcome_variable), " - ", location_short_name))) %>%
#     hc_add_series(data = timeseries_data, type = "line", name = "Data",
#                   hcaes(x = Date, y = PlotOutcome)) %>%
#     hc_add_series(data = model_data, type = "line", name = "Model",
#                   hcaes(x = Date, y = PlotOutcome, group = RunID)) %>%
#     hc_add_theme(hc_covid_theme)
#   
# }

# @export
# model_scenario_plot_hc = function(model, location_short_name = "NY", outcome_variable = "CumulativePositiveTests", log_scale = T) {
#   
#   # This data-wrangling part should probably be generalized:
#   location_id = model$location$LocationID[model$location$LocationShortName %in% location_short_name]
#   
#   min_date_ts = model$timeseries %>%
#     filter(CumulativePositiveTests >= 50)
#   
#   min_date = min(min_date_ts$Date)
#   
#   max_date = max(model$timeseries$Date)
#   
#   timeseries_data = model$timeseries %>%
#     filter(LocationID %in% location_id & CumulativePositiveTests > 50 & Date <= max_date & Date >= min_date)
#   
#   past_model = model$cali_augm_results %>% 
#     filter(LocationID %in% location_id & CumulativePositiveTests > 50 & Date <= max_date & Date >= min_date)
#   
#   scenarios_data = model$scenarios_augm_results %>%
#     filter(LocationID %in% location_id)
#   
#   model_data = rbind(past_model, scenarios_data)
#   
#   timeseries_data$PlotOutcome = timeseries_data[[outcome_variable]]
#   model_data$PlotOutcome = round(model_data[[outcome_variable]], digits = 2) 
#   
#   
#   if(log_scale){
#     timeseries_data$PlotOutcome = round(log10(timeseries_data$PlotOutcome), 2)
#     model_data$PlotOutcome = round(log10(model_data$PlotOutcome), 2)
#   }
#   
#   highchart() %>%
#     # The linke below format the dates:
#     #hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>%
#     hc_xAxis(type = "datetime") %>%
#     hc_yAxis(title = list(text = paste0(split_camel_case(outcome_variable), " - ", location_short_name))) %>%
#     hc_add_series(data = timeseries_data, type = "line", name = "Data",
#                   hcaes(x = Date, y = PlotOutcome)) %>%
#     hc_add_series(data = model_data, type = "line", name = "Model",
#                   hcaes(x = Date, y = PlotOutcome, group = RunID)) %>%
#     hc_add_theme(hc_covid_theme)
# }


# @export
# deaths_plot_hc = function(model, location_short_name = "NY", log_scale = T) {
#   
#   # This data-wrangling part should probably be generalized:
#   location_id = model$location$LocationID[model$location$LocationShortName %in% location_short_name]
#   
#   min_date_ts = model$timeseries %>%
#     filter(CumulativePositiveTests >= 50)
#   
#   min_date = min(min_date_ts$Date)
#   
#   max_date = max(model$timeseries$Date)
#   
#   timeseries_data = model$timeseries %>%
#     filter(LocationID %in% location_id & Date <= max_date & Date >= min_date)
#   
#   past_model = model$cali_augm_results %>% 
#     filter(LocationID %in% location_id & Date <= max_date & Date >= min_date)
#   
#   scenarios_data = model$scenarios_augm_results %>%
#     filter(LocationID %in% location_id)
#   
#   model_data = rbind(past_model, scenarios_data)
#   
#   
#   if(log_scale){
#     
#     timeseries_data$CumulativeDeaths = round(log10(timeseries_data$CumulativeDeaths), 2)
#     
#     model_data$CumulativeDeaths = round(log10(model_data$CumulativeDeaths), 2)
#     
#     model_data$CumulativeActualDeaths = round(log10(model_data$CumulativeActualDeaths), 2)
#     
#   }
#   
#   highchart() %>%
#     # The linke below format the dates:
#     #hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>%
#     hc_xAxis(type = "datetime") %>%
#     hc_yAxis(title = list(text = paste0( "Deaths - ", location_short_name))) %>%
#     hc_add_series(data = timeseries_data, type = "line", name = "Data",
#                   hcaes(x = Date, y = CumulativeDeaths)) %>%
#     hc_add_series(data = model_data, type = "line", name = "Model - Reported",
#                   hcaes(x = Date, y = CumulativeDeaths, group = RunID)) %>%
#     hc_add_series(data = model_data, type = "line", name = "Model - Actual",
#                   hcaes(x = Date, y = CumulativeActualDeaths, group = RunID)) %>%
#     
#     hc_add_theme(hc_covid_theme)
# }


# @export
# cases_plot_hc = function(model, location_short_name = "NY", log_scale = T) {
#   
#   # This data-wrangling part should probably be generalized:
#   location_id = model$location$LocationID[model$location$LocationShortName %in% location_short_name]
#   
#   min_date_ts = model$timeseries %>%
#     filter(CumulativePositiveTests >= 50)
#   
#   min_date = min(min_date_ts$Date)
#   
#   max_date = max(model$timeseries$Date)
#   
#   timeseries_data = model$timeseries %>%
#     filter(LocationID %in% location_id & Date <= max_date & Date >= min_date)
#   
#   past_model = model$cali_augm_results %>% 
#     filter(LocationID %in% location_id & Date <= max_date & Date >= min_date)
#   
#   scenarios_data = model$scenarios_augm_results %>%
#     filter(LocationID %in% location_id)
#   
#   model_data = rbind(past_model, scenarios_data)
#   
#   
#   if(log_scale){
#     
#     timeseries_data$CumulativePositiveTests = round(log10(timeseries_data$CumulativePositiveTests), 2)
#     
#     model_data$CumulativePositiveTests = round(log10(model_data$CumulativePositiveTests), 2)
#     
#     model_data$CumulativeRealCases = round(log10(model_data$CumulativeRealCases), 2)
#     
#   }
#   
#   highchart() %>%
#     # The linke below format the dates:
#     #hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>%
#     hc_xAxis(type = "datetime") %>%
#     hc_yAxis(title = list(text = paste0( "Cases - ", location_short_name))) %>%
#     hc_add_series(data = timeseries_data, type = "line", name = "Data",
#                   hcaes(x = Date, y = CumulativePositiveTests)) %>%
#     hc_add_series(data = model_data, type = "line", name = "Model - Reported",
#                   hcaes(x = Date, y = CumulativePositiveTests, group = RunID)) %>%
#     hc_add_series(data = model_data, type = "line", name = "Model - Actual",
#                   hcaes(x = Date, y = CumulativeRealCases, group = RunID)) %>%
#     
#     hc_add_theme(hc_covid_theme)
# }


# @export
# area_plot_hc = function(model, location_short_name = "NY", log_scale = T) {
#   
#   # This data-wrangling part should probably be generalized:
#   location_id = model$location$LocationID[model$location$LocationShortName %in% location_short_name]
#   
#   min_date_ts = model$timeseries %>%
#     filter(CumulativePositiveTests >= 50)
#   
#   min_date = min(min_date_ts$Date)
#   
#   max_date = max(model$timeseries$Date)
#   
#   
#   past_model = model$cali_augm_results %>% 
#     filter(LocationID %in% location_id & Date < max_date & Date >= min_date)
#   
#   scenarios_data = model$scenarios_augm_results %>%
#     filter(LocationID %in% location_id)
#   
#   model_data = rbind(past_model, scenarios_data)
#   
#   variables = c("Date", "S", "E", "P", "ISm", "ISs", "YSm", "YSs", "CurrentlyHospitalized", "CurrentlyInICU", "IA", "YA", "CumulativeDeaths", "R")
#   
#   variables = c("Date", "E", "P", "ISm", "ISs", "YSm", "YSs", "CurrentlyHospitalized", "CurrentlyInICU", "IA", "YA", "CumulativeDeaths")
#   
#   variables = c("Date", "P", "ISm", "ISs", "YSm", "YSs", "CurrentlyHospitalized", "CurrentlyInICU", "IA", "YA")
#   
#   
#   plot_data = model_data %>%
#     #  mutate(ISm + Y) %>%
#     select(one_of(variables)) %>%
#     tidyr::gather(Status, People, -Date) %>%
#     filter( as.integer(Date)%% 7 == 0)
#   
#   hchart(plot_data, "column", hcaes(x = Date, y = People, group = Status)) %>%
#     hc_yAxis(title = list(text =  location_short_name)) %>%
#     hc_plotOptions(column = list(
#       #dataLabels = list(enabled = FALSE),
#       stacking = "normal"
#       #enableMouseTracking = TRUE)
#     )) # %>%
#   #hc_add_theme(hc_covid_theme)
#   
# }

# @export
# model_scenario_stocks_plot = function(model, location_short_name = "NY") {
#   
#   # This data-wrangling part should probably be generalized:
#   location_id = model$location$LocationID[model$location$LocationShortName %in% location_short_name]
#   
#   min_date_ts = model$timeseries %>%
#     filter(CumulativePositiveTests >= 50)
#   
#   min_date = min(min_date_ts$Date)
#   
#   max_date = max(model$timeseries$Date)
#   
#   timeseries_data = model$timeseries %>%
#     filter(LocationID %in% location_id & CumulativePositiveTests > 50 & Date <= max_date & Date >= min_date)
#   
#   past_model = model$cali_augm_results %>% 
#     filter(LocationID %in% location_id & CumulativePositiveTests > 50 & Date <= max_date & Date >= min_date)
#   
#   scenarios_data = model$scenarios_augm_results %>%
#     filter(LocationID %in% location_id)
#   
#   model_data = rbind(past_model, scenarios_data)
#   
#   timeseries_data$PlotOutcome = timeseries_data[[outcome_variable]]
#   model_data$PlotOutcome = round(model_data[[outcome_variable]], digits = 2) 
#   
#   
#   if(log_scale){
#     timeseries_data$PlotOutcome = round(log10(timeseries_data$PlotOutcome), 2)
#     model_data$PlotOutcome = round(log10(model_data$PlotOutcome), 2)
#   }
#   
#   highchart() %>%
#     # The linke below format the dates:
#     #hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>%
#     hc_xAxis(type = "datetime") %>%
#     hc_yAxis(title = list(text = paste0(split_camel_case(outcome_variable), " - ", location_short_name))) %>%
#     hc_add_series(data = timeseries_data, type = "line", name = "Data",
#                   hcaes(x = Date, y = PlotOutcome)) %>%
#     hc_add_series(data = model_data, type = "line", name = "Model",
#                   hcaes(x = Date, y = PlotOutcome, group = RunID)) %>%
#     hc_add_theme(hc_covid_theme)
#   
# }


#----------------------------------------------------------------------------------------#
# Author: Tyna
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#
# @export
# strata_plot_hc = function(calib_model, sim_model, strat_list, desc_list, title ){
#   len = length(strat_list)
#   
#   df_calibrated = calib_model$cal_results %>% select(strat_list) %>% gather(strata, proportion, strat_list[4]:strat_list[len]) %>% mutate(type ="calibrated")
#   df_simulated = sim_model$scenarios_results %>% select(strat_list) %>% gather(strata, proportion, strat_list[4]:strat_list[len]) %>% mutate(type = "simulated")
#   
#   df <- rbind(df_calibrated, df_simulated) %>% filter(!(Date==Sys.Date() & type=="simulated"))
#   
#   
#   highchart() %>%
#     hc_xAxis(type = "datetime") %>%
#     hc_yAxis(title = list(text = title)) %>%
#     hc_add_series(data = filter(df, strata == strat_list[4]), type = "line", name = desc_list[1] , hcaes(x = Date, y = proportion, group = RunID)) %>%
#     hc_add_series(data = filter(df, strata == strat_list[5]), type = "line", name = desc_list[2] , hcaes(x = Date, y = proportion, group = RunID)) %>%
#     hc_add_series(data = filter(df, strata == strat_list[6]), type = "line", name = desc_list[3] , hcaes(x = Date, y = proportion, group = RunID)) %>%
#     hc_add_series(data = filter(df, strata == strat_list[7]), type = "line", name = desc_list[4] , hcaes(x = Date, y = proportion, group = RunID)) %>%
#     hc_add_series(data = filter(df, strata == strat_list[8]), type = "line", name = desc_list[5] , hcaes(x = Date, y = proportion, group = RunID)) %>%
#     hc_add_series(data = filter(df, strata == strat_list[9]), type = "line", name = desc_list[6] , hcaes(x = Date, y = proportion, group = RunID)) %>%
#     hc_add_series(data = filter(df, strata == strat_list[10]), type = "line", name = desc_list[7] , hcaes(x = Date, y = proportion, group = RunID)) %>%
#     hc_add_series(data = filter(df, strata == strat_list[11]), type = "line", name = desc_list[8] , hcaes(x = Date, y = proportion, group = RunID)) %>%
#     hc_add_series(data = filter(df, strata == strat_list[12]), type = "line", name = desc_list[9] , hcaes(x = Date, y = proportion, group = RunID)) %>%
#     hc_add_series(data = filter(df, strata == strat_list[13]), type = "line", name = desc_list[10] , hcaes(x = Date, y = proportion, group = RunID)) %>%
#     hc_add_series(data = filter(df, strata == strat_list[14]), type = "line", name = desc_list[11] , hcaes(x = Date, y = proportion, group = RunID)) # %>%  hc_add_theme(hc_covid_theme)
#   
#   
# }


#----------------------------------------------------------------------------------------#
# Author: Tyna
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#
# @export
# age_plots_hc <- function(calib_model, sim_model, strat, desc, index){
#   
#   data = rbind(calib_model, sim_model) %>% filter(!(Date==Sys.Date() & type=="simulated")) %>% 
#     mutate(pop.strata = ifelse(grepl("[A-Za-z]1$", strata), "0_19_healthy", 
#                                ifelse(grepl("[A-Za-z]2$", strata), "20_24_healthy", 
#                                       ifelse(grepl("[A-Za-z]3$", strata), "25_54_healthy", 
#                                              ifelse(grepl("[A-Za-z]4$", strata), "55_64_healthy", 
#                                                     ifelse(grepl("[A-Za-z]5$", strata), "65up_healthy", 
#                                                            ifelse(grepl("[A-Za-z]6$", strata), "0_19_chronic", 
#                                                                   ifelse(grepl("[A-Za-z]7$", strata), "20_24_chronic", 
#                                                                          ifelse(grepl("[A-Za-z]8$", strata), "25_54_chronic", 
#                                                                                 ifelse(grepl("[A-Za-z]9$", strata), "55_64_chronic", 
#                                                                                        ifelse(grepl("[A-Za-z]10$", strata), "65up_chronic", 
#                                                                                               ifelse(grepl("[A-Za-z]11$", strata), "HCW", NA ))))))))))))
#   
#   highchart() %>%
#     hc_xAxis(type = "datetime") %>% hc_yAxis(title = list(text = desc[i])) %>%
#     hc_add_series(data = filter(data, grepl(paste0("^", strat[i]), strata)), 
#                   type = "area" ,
#                   hcaes(x = Date,
#                         y = proportion,
#                         group = pop.strata)) %>%
#     hc_plotOptions(series = list(stacking="normal", 
#                                  marker = list(enabled = FALSE,
#                                                states = list(hover = list(enabled = FALSE))),
#                                  lineWidth = 0.5, lineColor = "white")) %>%  hc_add_theme(hc_covid_theme)
# }


# @export
# hc_state_map = function(model, outcome, map_json = usgeojson) {
#   # Define this function and move it out of here
#   state_data = model$timeseries %>%
#     filter(Date == max(Date)) %>%
#     left_join(model$location %>% select(LocationID, Name), by = "LocationID")
#   
#   # Defining highchart
#   highchart() %>%
#     hc_add_series_map(map_json, state_data, name = split_camel_case(outcome) ,
#                       value = outcome, joinBy = c("woename", "Name"),
#                       dataLabels = list(enabled = TRUE,
#                                         format = '{point.properties.postalcode}')) %>%
#     hc_colorAxis(hc = ., minColor = "#FFFFFF", maxColor = "#4b197c") %>%
#     #  hc_colorAxis(stops = colstops) %>%
#     hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
#     hc_mapNavigation(enabled = TRUE) %>%
#     hc_add_theme(hc_covid_theme)
#   
# }

#### HC Theme ####
# @export
# hc_covid_theme <-
#   hc_theme(
#     colors = c("#4b197c", "#434348", "#f89a3a"),
#     chart = list(
#       backgroundColor = "transparent",
#       style = list(fontFamily = "adelle")
#     ),
#     xAxis = list(
#       gridLineWidth = 1
#     )
#   )


#### ggplots ####
#' @import ggplot2 
#' @importFrom scales trans_breaks trans_format math_format
#' @export
plot.c19model = function(model, logscale = T, metric = "CumulativePositiveTests", absolute_value = F) {
  
  if(model$status != "calibrated") {
    cat_red_bullet("You need to calibrate your model before plotting any results.")
    return()
  }
  
  # Determine date in which first death occurred:
  selecting_start_date = model$timeseries %>%
    #filter(CumulativePositiveTests >= 100) %>%
    ungroup(.) %>%
    mutate(MinDate = min(Date), MaxDate = max(Date))
  
  start_date = selecting_start_date$MinDate[1]
  
  end_date = selecting_start_date$MaxDate[1]
  
  if(paste0(metric, "MovingAverage") %in% colnames(model$timeseries)) {
    # Use the Moving Average:
    model$timeseries$Outcome = model$timeseries[[paste0(metric, "MovingAverage")]] / model$timeseries$Population
    
  } else {
    #Don't use the moving average:
    model$timeseries$Outcome = model$timeseries[[metric]] / model$timeseries$Population
  }
  
  model$cali_augm_results$Outcome = model$cali_augm_results[[metric]] / model$cali_augm_results$Population
  
  calibration_information = model$scenarios %>%
    left_join(model$calibration, by = "LocationID") %>% 
    left_join(model$location %>% 
                select(LocationID, LocationShortName), by = "LocationID") %>%
    mutate(CalibratedWithDeaths = T0Coeff > 0 & T0Coeff == deaths_coeff,
           CalibratedWithCases = T0Coeff > 0 & T0Coeff == cases_coeff,
           CalibrationMethod = ifelse(CalibratedWithDeaths, "Deaths", ifelse(CalibratedWithCases, "Cases", "Pop. Density")))
  
  # The calibration_information table is a useful table for verification:
  #writexl::write_xlsx(calibration_information, "calibrationinfo.xlsx")
  
  # Filtering Plot Data:
  timeseries = model$timeseries %>% 
    filter(LocationID %in% model$location_ids & Date >= start_date)
  
  model_data = model$cali_augm_results %>% filter(Date >= start_date & Date <= end_date) %>%
    left_join(calibration_information %>% select(LocationID, CalibrationMethod), by = "LocationID")
  
  browser()
  
  plot = ggplot(model_data, mapping = aes(x=Date, y = Outcome)) +
    geom_line(mapping = aes(color = as.factor(CalibrationMethod), group = RunID, alpha=0.4)) +
    #geom_vline(xintercept = calibration_information$T0EndDeaths, show.legend = T,linetype="dotted") + 
    geom_path(data = timeseries, mapping = aes(x = lubridate::as_date(Date), y = Outcome), size=1.5) +
    facet_wrap(facets = "LocationShortName", scales = "free") + 
    guides(color=guide_legend(title="Calibration Method"))
  
  if (logscale) {
    
    plot = format_ggplot_y_continuous_scale(plot = plot, number_format = "percent", log_scale = T)
    
    # plot = plot + ggplot2::scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
    #                                      labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
    #   ylab(split_camel_case(paste0(metric, " (Log Scale)")))
    
  } else {
    plot = plot + 
      ylab(split_camel_case(paste0(metric, " %")))
  }
  
  covid19_theme_color(plot)
  
}

#' @export
plot_location_ts = function(model, location_short_names, variable_name) {
  plot = model$timeseries %>%
    #left_join(model$location, by = "LocationID") %>%
    filter(LocationShortName %in% location_short_names) %>%
    ggplot(., mapping = aes_string(x = "Date", y = variable_name, color = "LocationShortName")) + 
    geom_line() + 
    scale_y_continuous(labels = scientific_10) + 
    ylab(split_camel_case(variable_name)) + 
    guides(color=guide_legend(title="Location"))
  plot
  #covid19_theme_color(plot)
}


#### ggplot themes ####
#' @export
covid19_theme_color = function(plot) {
  plot + 
    ggsci::scale_color_aaas() +
    ggsci::scale_fill_aaas() + 
    covid19_theme()
}
#' @export
# This function returns a generic theme that can be added to plots, without changing colors
covid19_theme = function() {
  # hrbrthemes::theme_ipsum_tw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"))
  
}