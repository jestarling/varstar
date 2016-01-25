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
###roxygen2 Documentation for the est_mixing_props_hip list object:

#' List of k estimated class proportions for each feature of the hip data set.
#'
#' This object is the result of running the featureMixtureProportion(hip)
#' function.  It is designed to streamline the code run time for analysis of hip,
#' as the function takes some time to compute.
#'
#' @format A list object with 2118 elements; each element is vector of length k:
#' \describe{
#'   \item{list[[1]]}{Vector of estimated class proportions for feature 1}
#'   \item{list[[2]]}{Vector of estimated class proportions for feature 2}
#'   ...
#' }
"est_mixing_props_hip"

#---------------------------------------------------------
###roxygen2 Documentation for the est_mixing_props_linear list object:

#' List of k estimated class proportions for each feature of the linear data set.
#'
#' This object is the result of running the featureMixtureProportion(linear)
#' function.  It is designed to streamline the code run time for analysis of
#' linear, as the function takes some time to compute.
#'
#' @format A list object with 5509 elements; each element is vector of length k:
#' \describe{
#'   \item{list[[1]]}{Vector of estimated class proportions for feature 1}
#'   \item{list[[2]]}{Vector of estimated class proportions for feature 2}
#'   ...
#' }
"est_mixing_props_linear"