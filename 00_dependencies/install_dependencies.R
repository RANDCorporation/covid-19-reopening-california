
#------------------------------------------------------------------------------#
# Code for the paper: Reopening California
#                     Seeking Robust, Non-Dominated COVID-19 Exit Strategies.
# 
# Author: Pedro Nascimento de Lima
# Copyright (C) 2021 by The RAND Corporation
# See LICENSE.txt and README.txt for information on usage and licensing
#------------------------------------------------------------------------------#

# Install these packages:
required_packages = c("dplyr", "tidyr", "ggplot2", "parallel", "doParallel", "MASS", "data.table", "remotes", "data.table", "foreach", "remotes")

install.packages(required_packages)


# Then install our package:
# The c19randepimod package contains generalized functions for out c19 model class.
library(remotes)

remotes::install_local("./00_dependencies/c19randepimod_1.0.0.tar.gz")

