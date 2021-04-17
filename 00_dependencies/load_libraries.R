
#------------------------------------------------------------------------------#
# Code for the paper: Reopening Under Deep Uncertainty: 
#                     Seeking Robust, Non-Dominated COVID-19 Exit Strategies.
# 
# Author: Pedro Nascimento de Lima
# Copyright (C) 2021 by The RAND Corporation
# See LICENSE.txt and README.txt for information on usage and licensing
#------------------------------------------------------------------------------#

# Load dependencies:
# Install these libraries before running this code.

library(c19randepimod)
library(imabc)
library(MASS)
library(data.table)
library(dplyr)
library(parallel)
library(foreach)
library(doParallel)
library(tidyr)
library(ggplot2)


# Sourcing functions needed for IMABC calibration:
source("./c19randepimod/R/class_c19model.R")
source("./c19randepimod/R/class_c19model_02_prepare_inputs.R")
source("./c19randepimod/R/class_c19model_03_calibrate.R")
source("./c19randepimod/R/class_c19model_04_run_model.R")
source("./c19randepimod/R/utils_misc.R")