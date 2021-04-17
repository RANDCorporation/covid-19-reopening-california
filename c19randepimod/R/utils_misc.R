

#----------------------------------------------------------------------------------------#
# c19randepimod R package
# This is the R package behind RAND's COVID-19 State policy tool and related papers.
# 
# R Package Author: Pedro Nascimento de Lima
# Model Author: Raffaele Vardavas.
# Copyright (C) 2021 by The RAND Corporation
# See LICENSE.txt and README.txt for information on usage and licensing
#----------------------------------------------------------------------------------------#

#### Auxiliary Functions -------------------------------------------------------------####

#----------------------------------------------------------------------------------------#
# Author: Pedro Nascimento de Lima
# Purpose: This File contains miscelaneous functions used across the package and he model.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#



#----------------------------------------------------------------------------------------#
# Function: Safe Division
# Purpose: Performs division even when denominator is zero.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#
#' @export
safe_division = function(num, denom) {
  
  result_length = max(length(num), length(denom))
  
  if(length(denom) == 1) {
    denom = rep(denom, result_length)
  }
  
  ifelse(denom == 0, numeric(result_length), num/denom)
  
}



#----------------------------------------------------------------------------------------#
# Function: Pert Distribution Quantiles
# Purpose: Returns the quantiles of a pert distribution.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#
#' @importFrom stats qbeta
qpert = function(p, x.min, x.mode=NULL, x.max,lambda = 4, mu=NULL){
  # See http://www.riskamp.com/beta-pert
  
  if(!is.null(mu) & is.null(x.mode)) {
    x.mode<- (mu*( lambda + 2 )- (x.min + x.max))/lambda
    if(x.mode>x.max) x.mode <- min(mu,x.max)
    if(x.mode<x.min) x.mode <- x.min
  }
  if(is.null(mu) & is.null(x.mode)) stop( "invalid parameters" );
  if( x.min > x.max || x.mode > x.max || x.mode < x.min ) stop( "invalid parameters" );
  
  x.range <- x.max - x.min;
  if( x.range == 0 ) return( rep( x.min, n ));
  
  mu <- ( x.min + x.max + lambda * x.mode ) / ( lambda + 2 );
  
  # special case if mu == mode
  if( abs(mu - x.mode)/x.mode < 0.00001 ){
    v <- ( lambda / 2 ) + 1
  }else {
    v <- (( mu - x.min ) * ( 2 * x.mode - x.min - x.max )) /
      (( x.mode - mu ) * ( x.max - x.min ));
  }
  
  w <- ( v * ( x.max - mu )) / ( mu - x.min );
  
  return ( qbeta( p, v, w ) * x.range + x.min )
}




#----------------------------------------------------------------------------------------#
# Function: Compute Testing Rates Moving Averages
# Purpose: Returns moving averages of testing rates vectors
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#
#' @importFrom zoo rollapply
testing_moving_average = function(vector, width = 3) {
  # This Testing Moving average always start with 0:
  zeros = numeric(width - 1)
  averages = zoo::rollapply(vector, width = width, FUN = mean)
  c(zeros, averages)
}



#### Formatting Functions ####

split_camel_case = function(string_vector) {
  gsub("([A-Z])", " \\1", string_vector)  
}

format_for_humans <- function(x, digits = 3){
  grouping <- pmax(floor(log(abs(x), 1000)), 0)
  paste(signif(x / (1000 ^ grouping), digits = digits),
        c('', 'K', 'M', 'B', 'T')[grouping + 1],sep = " ")
}

format_percentage_for_humans = function(x, digits = 1){
  paste(round(100 * x, digits), "%", sep = " ")
}

format_currency_for_humans = function(x, currency = "$", digits = 3) {
  paste(currency, format_for_humans(x, digits = digits), sep = " ")
}

format_ggplot_y_continuous_scale = function(plot, number_format = "number", log_scale = F) {
  
  if(log_scale) {
    plot = plot + ggplot2::scale_y_log10()
  }
  
  if(number_format == "number") {
    plot = plot + ggplot2::scale_y_continuous(labels = format_for_humans)
  } else if (number_format == "percent"){
    plot = plot + scale_y_continuous(labels = scales::percent)
    # Alternatively, I could use my function:
    #plot = plot + ggplot2::scale_y_continuous(labels = format_percentage_for_humans)
  } else if (number_format == "dollar"){
    plot = plot + ggplot2::scale_y_continuous(labels = format_currency_for_humans)
  }
  
  plot
  
}

scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}


#### Data Retrieving Functions ####

get_location_short_names = function(model, location_ids) {
  model$location$LocationShortName[model$location$LocationID %in% location_ids]
}

get_location_names = function(model, location_ids) {
  model$location$Name[model$location$LocationID %in% location_ids]
}

get_location_ids = function(model, location_shortnames) {
  model$location$LocationID[model$location$LocationShortName %in% location_shortnames]
}

get_current_intervention_id = function(model, location_id) {
  ts = model$timeseries %>%
    filter(LocationID == location_id & Date == max(Date))
  ts$PortfolioID
}

get_portfolio_descriptions = function(model, portfolio_ids) {
  model$npiportfolio$PortfolioDescription[model$npiportfolio$PortfolioID %in% portfolio_ids]
}

get_portfolio_id = function(model, portfolio_description) {
  model$npiportfolio$PortfolioID[model$npiportfolio$PortfolioDescription == portfolio_description]
}



# Messages Symbols
#' @importFrom cli cat_bullet
#' @export
cat_green_tick <- function(...){
  cat_bullet(
    ..., 
    bullet = "tick", 
    bullet_col = "green"
  )
}

#' @importFrom cli cat_bullet
#' @export
cat_red_bullet <- function(...){
  cat_bullet(
    ..., 
    bullet = "bullet",
    bullet_col = "red"
  )
}

#' @importFrom cli cat_bullet
#' @export
cat_info <- function(...){
  cat_bullet(
    ..., 
    bullet = "arrow_right",
    bullet_col = "grey"
  )
}

#' Check Model nas
#'
#' @param model the c19model object
check_model_nas = function(model) {
  nas = 0
  for(x in names(model)) {
    if(!(typeof(model[[x]]) == "closure")) {
      if(any(is.na(model[[x]]))) {
        #warning(paste0("Warning: There are NAs in your Table: ", x), call. = F)
        cat_red_bullet(paste0("Warning: There are NAs in your Table: ", x))
        nas = nas + 1
      }  
    }
  }
  
  if(nas == 0) {
    cat_green_tick("No NA found in our model object.")
  }
}





#' Set up a COID-19 Folder
#'
#' Use this function to create a folder structure for the model.
#'
#' @param path The path where you want to create the folder. If the path is not a directory, the function will create a /c19analysis inside this path. If missing, defaults to the current working directory.
#' @param overwrite If TRUE, will overwrite an existing folder.
#'
#' @export
setup_c19_folder = function(path, overwrite = F) {
  
  # If path is missing, I will assume it is in the current directory
  
  if(missing(path)) {
    path = getwd()
  }
  
  app_folder = path
  
  data_folder = paste0(app_folder, "/inputs")
  
  # If path doesnt exists, or you want to overwrite:
  if(!dir.exists(app_folder) | overwrite) {
    
    # Create the Directory:
    dir.create(app_folder)
    
    app_files_location = system.file("template_analysis", package = "c19randepimod")
    
    # Copy Template Analysis Folder:
    
    file.copy(from = app_files_location,
              to = app_folder,
              overwrite = overwrite,
              recursive = T)
    
    cat_green_tick(paste0("Folder Created at: ", app_folder))
    
  } else {
    
    cat_red_bullet(paste0("Will not overwrite the ", app_folder, " folder without permition. Please change the path or use overwrite = TRUE if you really want to overwrite this folder."))
  
  }
  
}




# My simplified version of the assert_that function:
# This is being used because of RAND's licensing process.
# revert back to asserthat when it changes its license.
check_that = function(condition, msg=NULL){
  
  if(!condition){
    
    message(msg)  
    stopifnot(condition)
  }
      
    
}



