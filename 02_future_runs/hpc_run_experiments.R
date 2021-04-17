
#------------------------------------------------------------------------------#
# Code for the paper: Reopening California
#                     Seeking Robust, Non-Dominated COVID-19 Exit Strategies.
# 
# Author: Pedro Nascimento de Lima
# Copyright (C) 2021 by The RAND Corporation
# See LICENSE.txt and README.txt for information on usage and licensing
#------------------------------------------------------------------------------#



# Loading Libraries -------------------------------------------------------
library(dplyr)
library(c19randepimod)
source("./02_future_runs/setup_rdm_experiments.R")
# Read experiment model from file:
experiment_model = readRDS(file = "./02_future_runs/outputs/experiment_model_rdm.rds")


# Taking job argments -----------------------------------------------------
# STARTING_ROW ENDING_ROW N_CORES
args = commandArgs(trailingOnly=TRUE)

starting_row = strtoi(args[1])
ending_row = strtoi(args[2])
n_cores = strtoi(args[3])
results_dir = args[4]

cat(paste0('Args: ', starting_row, ', ', ending_row, ', ', n_cores, '\n'))

# Parallel model to use:
PARALLEL_MODE = "FORK"

# Filtering the only the experiments of interest:
experiment_model$future_experimental_design = experiment_model$future_experimental_design %>%
  dplyr::filter(ExperimentID %in% starting_row:ending_row)


# Evaluating Experiments --------------------------------------------------
results_rdm = evaluate_experiment(model = experiment_model, 
                                  runs = starting_row:ending_row,
                                  test_run =F, sim_end_date = "2022-01-30", 
                                  n_cores = n_cores,
                                  parallel_mode = PARALLEL_MODE, write_csv = F)


# Saving the Final and the Full results dataset separately ----------------
final_results = results_rdm %>%
  filter(Date == max(Date))

full_results_dir = paste0("./02_future_runs/outputs/", results_dir)

results_dir_full = paste0(full_results_dir, "_full")
results_dir_final = paste0(full_results_dir, "_final")

dir.create(results_dir_full, showWarnings=FALSE)
dir.create(results_dir_final, showWarnings=FALSE)

results_file_full = paste0(results_dir_full, '/results_', starting_row, '_', ending_row, '.rds')
results_file_final = paste0(results_dir_final, '/results_', starting_row, '_', ending_row, '.rds')


# Writing results ---------------------------------------------------------
cat(paste0("Writing Results to ", results_file_final, '\n'))
saveRDS(final_results, file=results_file_final)

cat(paste0("Writing Results to ", results_file_full, '\n'))
saveRDS(results_rdm, file=results_file_full)
