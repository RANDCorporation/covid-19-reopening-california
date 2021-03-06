% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_c19model.R
\name{set_calibrated_k}
\alias{set_calibrated_k}
\title{Set Calibrated K Factor}
\usage{
set_calibrated_k(
  model,
  location_ids,
  n_lhs = 2,
  minR0 = 2,
  maxR0 = 4,
  scenarios = NULL
)
}
\arguments{
\item{model}{a model object of the c19model class}

\item{location_ids}{vector of locations for which to compute the k factor}

\item{n_lhs}{integer defining the number of samples to create}

\item{minR0}{Minimum R0 that will be used for states without enough data}

\item{maxR0}{Maximum R0 that will be used for locations without enough data}
}
\value{
a model object of the c19model class
}
\description{
The models built out based on Raffaelle Vardava's AMSHI model need a proportionality factor k to run. This function computes this factor for a set of locations, while creating a latin hypercube sample of unknown parameters.
}
