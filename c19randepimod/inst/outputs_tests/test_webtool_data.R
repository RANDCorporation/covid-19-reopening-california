
### WebTool Data Tests Suite
# Author: Pedro Nascimento de Lima
# This File Contains Tests for the outputs that go into the 

# This File
# This Functions Verifies Outputs from the generate_webtool_data
# This function does not runs or tests R functions directly.

library(testthat)

# Files

files_path = "C:/dev/covid19-cloudsearch-tool/data_files/"

## Defining file names:
locations_file = paste0(files_path, "locations.csv")
current_aggregate_file = paste0(files_path, "current_aggregate.csv")
current_timeseries_file = paste0(files_path, "current_timeseries.csv")
intervention_dates_file = paste0(files_path, "intervention_dates.csv")
projected_aggregate_file = paste0(files_path, "projected_aggregate.csv")
projected_time_series_file = paste0(files_path, "projected_time_series.csv")

# Expected Names:
locations_names = c("location_id", "location_type", "location_name", "intervention_level", 
  "intervention_description", "testing_status", "interventions_start")

current_timeseries_names = c("location_id", "date", "cases_num", "cases_pct", "hospitalizations_num", 
                             "icu_num", "population", "fatalities_num", "fatalities_pct", 
                             "hospitalizations_pct", "icu_pct", "init_unemployment_num", "init_unemployment_pct", 
                             "total_unemployment_num", "total_unemployment_pct", "current_cases_num", 
                             "current_fatalities_num")

current_aggregate_names = c("location_id", "cases_num", "cases_pct", "hospitalizations_num", 
                            "icu_num", "population", "fatalities_num", "fatalities_pct", 
                            "hospitalizations_pct", "icu_pct", "init_unemployment_num", "init_unemployment_pct", 
                            "total_unemployment_num", "total_unemployment_pct", "current_cases_num", 
                            "current_cases_pct", "current_fatalities_num", "current_fatalities_pct", 
                            "cumulative_hospitalizations_num", "cumulative_hospitalizations_pct", 
                            "cumulative_icu_num", "cumulative_icu_pct")

intervention_dates_names = c("intervention_date_id", "intervention_date")

projected_aggregate_names = c("location_id", "portfolio_id", "intervention_level", "intervention_length", 
                              "intervention_date_id", "gsp_num", "gsp_pct", "hh_income_num", 
                              "hh_income_pct")

projected_time_series_names = c("location_id", "portfolio_id", "future_scenario_id", "date", 
                                "intervention_level", "intervention_length", "intervention_date_id", 
                                "cases_num", "cumulative_cases_num", "hospitalizations_num", 
                                "current_hospitalizations_num", "fatalities_num", "cumulative_fatalities_num", 
                                "icu_num", "current_icu_num", "cases_lower_bound", "cumulative_cases_lower_bound", 
                                "hospitalizations_lower_bound", "current_hospitalizations_lower_bound", 
                                "fatalities_lower_bound", "cumulative_fatalities_lower_bound", 
                                "icu_lower_bound", "current_icu_lower_bound", "cases_upper_bound", 
                                "cumulative_cases_upper_bound", "hospitalizations_upper_bound", 
                                "current_hospitalizations_upper_bound", "fatalities_upper_bound", 
                                "cumulative_fatalities_upper_bound", "icu_upper_bound", "current_icu_upper_bound")


# Expected Classes:

locations_classes = c(location_id = "integer", location_type = "character", location_name = "character", 
                     intervention_level = "integer", intervention_description = "character", 
                     testing_status = "character", interventions_start = "character"
)

current_aggregate_classes = c(location_id = "integer", cases_num = "integer", cases_pct = "numeric", 
                              hospitalizations_num = "integer", icu_num = "integer", population = "integer", 
                              fatalities_num = "integer", fatalities_pct = "numeric", hospitalizations_pct = "numeric", 
                              icu_pct = "numeric", init_unemployment_num = "integer", init_unemployment_pct = "numeric", 
                              total_unemployment_num = "integer", total_unemployment_pct = "numeric", 
                              current_cases_num = "integer", current_cases_pct = "numeric", 
                              current_fatalities_num = "integer", current_fatalities_pct = "numeric", 
                              cumulative_hospitalizations_num = "integer", cumulative_hospitalizations_pct = "numeric", 
                              cumulative_icu_num = "integer", cumulative_icu_pct = "numeric"
)

current_timeseries_classes = c(location_id = "integer", date = "character", cases_num = "integer", 
                               cases_pct = "numeric", hospitalizations_num = "integer", icu_num = "integer", 
                               population = "integer", fatalities_num = "integer", fatalities_pct = "numeric", 
                               hospitalizations_pct = "numeric", icu_pct = "numeric", init_unemployment_num = "integer", 
                               init_unemployment_pct = "numeric", total_unemployment_num = "integer", 
                               total_unemployment_pct = "numeric", current_cases_num = "integer", 
                               current_fatalities_num = "integer")

intervention_dates_classes = c(intervention_date_id = "integer", intervention_date = "character"
)

projected_aggregate_classes = c(location_id = "integer", portfolio_id = "integer", intervention_level = "integer", 
                                intervention_length = "integer", intervention_date_id = "integer", 
                                gsp_num = "numeric", gsp_pct = "numeric", hh_income_num = "integer", 
                                hh_income_pct = "integer")

projected_time_series_classes = c(location_id = "integer", portfolio_id = "integer", future_scenario_id = "integer", 
                                  date = "character", intervention_level = "integer", intervention_length = "integer", 
                                  intervention_date_id = "integer", cases_num = "numeric", cumulative_cases_num = "numeric", 
                                  hospitalizations_num = "numeric", current_hospitalizations_num = "numeric", 
                                  fatalities_num = "numeric", cumulative_fatalities_num = "numeric", 
                                  icu_num = "numeric", current_icu_num = "numeric", cases_lower_bound = "numeric", 
                                  cumulative_cases_lower_bound = "numeric", hospitalizations_lower_bound = "numeric", 
                                  current_hospitalizations_lower_bound = "numeric", fatalities_lower_bound = "numeric", 
                                  cumulative_fatalities_lower_bound = "numeric", icu_lower_bound = "numeric", 
                                  current_icu_lower_bound = "numeric", cases_upper_bound = "numeric", 
                                  cumulative_cases_upper_bound = "numeric", hospitalizations_upper_bound = "numeric", 
                                  current_hospitalizations_upper_bound = "numeric", fatalities_upper_bound = "numeric", 
                                  cumulative_fatalities_upper_bound = "numeric", icu_upper_bound = "numeric", 
                                  current_icu_upper_bound = "numeric")



context("checking files")

test_that("all csv files are in the folder", {
  
  # Th
  expect_true(file.exists(locations_file))
  expect_true(file.exists(current_aggregate_file))
  expect_true(file.exists(current_timeseries_file))
  expect_true(file.exists(intervention_dates_file))
  expect_true(file.exists(projected_aggregate_file))
  expect_true(file.exists(projected_time_series_file))
  
})


locations = read.csv(locations_file, stringsAsFactors = F)
current_aggregate = read.csv(current_aggregate_file, stringsAsFactors = F)
current_timeseries = read.csv(current_timeseries_file, stringsAsFactors = F)
intervention_dates = read.csv(intervention_dates_file, stringsAsFactors = F)
projected_aggregate = read.csv(projected_aggregate_file, stringsAsFactors = F)
projected_time_series = read.csv(projected_time_series_file, stringsAsFactors = F)


test_that("all files are read as dataframes", {
  
  expect_true(is.data.frame(locations))
  expect_true(is.data.frame(current_aggregate))
  expect_true(is.data.frame(current_timeseries))
  expect_true(is.data.frame(intervention_dates))
  expect_true(is.data.frame(projected_aggregate))
  expect_true(is.data.frame(projected_time_series))
  
  
})

# This Function Checks if there are negative numbers that are different than -100.
# -100 means NAs for the Webtool API
check_wrong_negatives  = function(data_frame, number = -100) {
  
  vector_check_negatives_different_than = function(vector, number) {
    vector = vector[[1]]
    if (!is.numeric(vector)) {
      FALSE
    } else {
      any(vector < 0 & vector != number)
    }
  }
  
  any(sapply(current_timeseries, vector_check_negatives_different_than, number = number))
  
}

check_outside_bounds = function(vector, min, max) {
  any(vector > max | vector < min)
}



context("locations file")

test_that("locations file seems integral", {
  
  expect_equal(names(locations), locations_names)
  expect_equal(sapply(locations, class), locations_classes)
  expect_equal(nrow(locations), 51)
  expect_false(any(is.na(locations)))
  expect_false(check_wrong_negatives(locations))
  
})


context("current timeseries file")

test_that("current_timeseries file seems integral", {
  
  expect_equal(names(current_timeseries), current_timeseries_names)
  
  expect_equal(sapply(current_timeseries, class), current_timeseries_classes)
  
  ts_rows = length(unique(locations$location_id)) * length(unique(current_timeseries$date))
  
  expect_equal(nrow(current_timeseries), ts_rows)
  
  expect_false(any(is.na(current_timeseries)))
  
  expect_false(check_wrong_negatives(current_timeseries))
  
  
})

context("current aggregate file")

test_that("current_aggregate file seems integral", {
  
  expect_equal(names(current_aggregate), current_aggregate_names)
  
  expect_equal(sapply(current_aggregate, class), current_aggregate_classes)
  
  expect_equal(nrow(current_aggregate), nrow(locations))
  
  expect_false(any(is.na(current_aggregate)))
  
  expect_false(check_wrong_negatives(current_aggregate))
  
})


context("intervention_dates file")

test_that("intervention_dates file seems integral", {
  
  expect_equal(names(intervention_dates), intervention_dates_names)
  
  expect_equal(sapply(intervention_dates, class), intervention_dates_classes)
  
  expect_true(nrow(intervention_dates) > 2)
  
  expect_false(any(is.na(intervention_dates)))
  
  expect_false(check_wrong_negatives(intervention_dates))
  
})


context("projected_time_series file")

test_that("projected_time_series file seems integral", {
  
  expect_equal(names(projected_time_series), projected_time_series_names)
  
  expect_equal(sapply(projected_time_series, class), projected_time_series_classes)
  
  n_rows_projected_timeseries = length(unique(projected_time_series$date)) * length(unique(locations$location_id)) * length(unique(intervention_dates$intervention_date_id)) *  length(unique(projected_time_series$portfolio_id)) 
  
  expect_equal(nrow(projected_time_series), n_rows_projected_timeseries)
  
  expect_false(any(is.na(projected_time_series)))
  
  expect_false(check_wrong_negatives(projected_time_series))
  
  # Checking Order of Magnitude of Some Numbers:
  
  # Fatalities
  expect_false(check_outside_bounds(projected_time_series$fatalities_num, min = 0, max = 1200))
  expect_false(check_outside_bounds(projected_time_series$fatalities_lower_bound, min = 0, max = 800))
  expect_false(check_outside_bounds(projected_time_series$fatalities_upper_bound, min = 0, max = 2500))
  
  # Cases
  expect_false(check_outside_bounds(projected_time_series$cases_num, min = 0, max = 9500))
  expect_false(check_outside_bounds(projected_time_series$cases_lower_bound, min = 0, max = 9000))
  expect_false(check_outside_bounds(projected_time_series$cases_upper_bound, min = 0, max = 13000))
  
  # Hospitalizations:
  expect_false(check_outside_bounds(projected_time_series$current_hospitalizations_num, min = 0, max = 65000))
  expect_false(check_outside_bounds(projected_time_series$current_hospitalizations_lower_bound, min = 0, max = 60000))
  expect_false(check_outside_bounds(projected_time_series$current_hospitalizations_upper_bound, min = 0, max = 65000))
  
  # ICU Numbers:
  expect_false(check_outside_bounds(projected_time_series$current_icu_num, min = 0, max = 8000))
  expect_false(check_outside_bounds(projected_time_series$current_icu_lower_bound, min = 0, max = 7000))
  expect_false(check_outside_bounds(projected_time_series$current_icu_upper_bound, min = 0, max = 9000))
  
})

context("projected_aggregate file")

test_that("projected_aggregate file seems integral", {
  
  expect_equal(names(projected_aggregate), projected_aggregate_names)
  
  expect_equal(sapply(projected_aggregate, class), projected_aggregate_classes)
  
  n_rows_projected_aggregate = length(unique(locations$location_id)) * length(unique(intervention_dates$intervention_date_id)) *  length(unique(projected_aggregate$portfolio_id)) 
  
  expect_equal(nrow(projected_aggregate), n_rows_projected_aggregate)
  
  expect_false(any(is.na(projected_aggregate)))
  
  expect_false(check_wrong_negatives(projected_time_series[,c("hh_income_pct")]))
  
  # Checking Economic Variable Bounds:
  
  expect_false(check_outside_bounds(projected_aggregate$gsp_num, min = -200000, max = 0))
  
  expect_false(check_outside_bounds(projected_aggregate$gsp_pct, min = 0, max = 0.2))
  
})



context("additional checks")


# This test is no longer active - The webtool deals with this issue in the front-end.

# test_that("cumulative hosp and icus are lower than current", {
#   
#   cum_greater_than_zero = (current_aggregate$cumulative_hospitalizations_num > 0)
#   cum_smaller_than_current = (current_aggregate$cumulative_hospitalizations_num < current_aggregate$hospitalizations_num)
#   
#   # Checking 
#   wrong_cumulative_hospitalizations = cum_greater_than_zero & cum_smaller_than_current
#   any(wrong_cumulative_hospitalizations)
#   
#   cum_greater_than_zero_icus = (current_aggregate$cumulative_icu_num > 0)
#   cum_smaller_than_current_icus = (current_aggregate$cumulative_icu_num < current_aggregate$icu_num)
#   
#   wrong_cumulative_icus = cum_greater_than_zero_icus & cum_smaller_than_current_icus
#   
#   expect_false(any(wrong_cumulative_hospitalizations))
#   
#   expect_false(any(wrong_cumulative_icus))
#   
# })