
#------------------------------------------------------------------------------#
# Code for the paper: Reopening California
#                     Seeking Robust, Non-Dominated COVID-19 Exit Strategies.
# 
# Author: Pedro Nascimento de Lima
# Copyright (C) 2021 by The RAND Corporation
# See LICENSE.txt and README.txt for information on usage and licensing
#------------------------------------------------------------------------------#

#### Setup RDM Experiments -------------------------------------------------####


# Load libraries and experimental design function:
library(dplyr)
library(c19randepimod)
source("./02_future_runs/setup_rdm_experiments.R")
# Read best fit model from file
best_fit_models = readRDS(file = "./01_calibration/outputs/best_fit_models.rds")


# Setting up the experiment:
set.seed(12345678)

experiment_model = setup_rdm_experiments(model_object = best_fit_models, 
                                         model_class_file = "model_class.R", 
                                         n_new_lhs = 100, 
                                         output_path = "./02_future_runs/outputs/")


# Defining time tolerance in minutes:
experiment_model$timetolerance = 15 * 60


# This vector contains all experiment ids, and can be used to create batches if needed:
experiments = 1:length(experiment_model$future_experimental_design$ExperimentID)

# Number of cores to be used in each node:
N_CORES = 4

# Parallel model to use:
PARALLEL_MODE = "FORK"


# Here is the point where we would parallelize across nodes:

# Run evaluate experiment for a set of experiments:
# Run this function in each node, selecting a subset of runs
results_rdm = evaluate_experiment(model = experiment_model, 
                                  # here, select a subset of the experiments:
                                  runs = 1:10,
                                  test_run =F, sim_end_date = "2022-03-01", 
                                  n_cores = N_CORES, 
                                  parallel_mode = PARALLEL_MODE, write_csv = F)


# save raw results if needed later:


# select relevant results to consolidate across runs:
results_to_consolidate = results %>%
  filter(Date == max(Date))


