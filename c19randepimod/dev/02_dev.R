# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "lhs" )
usethis::use_package( "ggplot2" )
usethis::use_package( "readxl" )
usethis::use_package( "ggsci" )
usethis::use_package( "parallel" )
usethis::use_package( "lubridate" )
usethis::use_package( "here" )
usethis::use_package( "scales" )
usethis::use_package( "glue" )
usethis::use_package( "stringr" )
usethis::use_package( "deSolve" )
usethis::use_package( "tidyr" )
usethis::use_package( "testthat" )
usethis::use_package( "pdftools" )


## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "name_of_module1" ) # Name of the module
golem::add_module( name = "name_of_module2" ) # Name of the module

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "update_daily_timeseries" ) 
golem::add_fct( "generate_webtool_data" ) 
golem::add_fct( "prepare_inputs" ) 
golem::add_fct( "get_augmented_inputs" ) 
golem::add_fct( "c19model" ) 
golem::add_fct( "calibrate" ) 
golem::add_fct( "solve_model" ) 
golem::add_fct( "run_model" ) 
golem::add_fct( "compute_outputs" ) 
golem::add_fct( "update_and_calibrate" ) 
golem::add_fct( "update_and_calibrate" ) 
golem::add_fct( "plots" ) 
golem::add_fct( "compute_regret" ) 

golem::add_utils( "misc" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("c19randepimod")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

