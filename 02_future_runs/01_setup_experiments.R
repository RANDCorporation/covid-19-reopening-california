
#------------------------------------------------------------------------------#
# Code for the paper: Reopening Under Deep Uncertainty: 
#                     Seeking Robust, Non-Dominated COVID-19 Exit Strategies.
# 
# Author: Pedro Nascimento de Lima
# Copyright (C) 2021 by The RAND Corporation
# See LICENSE.txt and README.txt for information on usage and licensing
#------------------------------------------------------------------------------#

library(c19randepimod)
library(dplyr)
source("./02_future_runs/setup_rdm_experiments.R")
# Read best fit model from file
calibrated_model = readRDS(file = "./01_calibration/outputs/calibrated_model.rds")

# Setting up the experiment:
set.seed(12345678)

calibrated_model$timetolerance = 15 * 60

experiment_model = setup_rdm_experiments(model_object = calibrated_model, 
                                         model_class_file = "model_class.R", 
                                         n_new_lhs = 200, 
                                         output_path = "./02_future_runs/outputs/")

# View(experiment_model$future_experimental_design)
