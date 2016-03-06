#---------------------------------------------------------
###roxygen2 Documentation for the hip data set:

#' Hipparcos variable star data.
#'
#' A dataset containing the classes and other features of 2118
#' variable stars.
#'
#' @format A data frame with 2118 rows and 144 variables:
#' \describe{
#'   \item{class}{variable star classification}
#'   ...
#' }
#' @source \url{http://adsabs.harvard.edu/abs/2012ada..confE..15S}
"hip"

#---------------------------------------------------------
###roxygen2 Documentation for the linear data set:

#' LINEAR variable star data.
#'
#' A dataset containing the classes and other features of 2118
#' variable stars.  The features are derived from the periodic light
#' curves, with raw data available in the second source link.
#'
#' @format A data frame with 5509 rows and 144 variables:
#' \describe{
#'   \item{class}{variable star classification}
#'   ...
#' }
#' @source \url{http://adsabs.harvard.edu/abs/2012ada..confE..15S}
#' @source \url{http://iopscience.iop.org/1538-3881/146/4/101/suppdata/aj482492t5_mrt.txt}
"linear"

#---------------------------------------------------------
###roxygen2 Documentation for the em_by_feature list object:

#' List containing output of the EM algorithm run for each individual feature.
#'
#' This object is the result of running emAllFeatures function for data sets each containing
#' the class column and the single feature.  Generated using maxiter=1000, conv=.00001.
#'
#' @format A list object with four elements:
#' \describe{
#'   \item{$est_mixing_props}{Matrix of estimated class proportions (cols) for each feature (rows)}
#'   \item{$feature_MSEs}{Vector of MSEs for each feature, for estimated props. vs unknown props.}
#'   \item{$feature_iter}{Vector of number of iterations to EM convergence for each feature.}
#'   \item{$feature_log}{List of vectors of log-likelihood functions for each feature..}
#'   ...
#' }
"em_by_feature"

#---------------------------------------------------------
###roxygen2 Documentation for the sample_model object:

#' Sample model using weighted data, out of the box random forest, EM feature selection.
#' Error rate of 18% for classifying linear data using hip data.
#'
#' @format A list object containing model components.
#' 
"sample_model"

