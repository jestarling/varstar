#-------------------------------------------------------------
# Package: varstar
# Function: featureMixtureProportion(df)

#Purpose: Estimate mixing proportions for each feature of an unknown
#	data set.  (Calls emCalcMix function for each feature.)

# Estimates mixture proportions for a features of observations divided into 5 classes. 
# df is a data frame with the first column "class", with levels defined for the 
# variable star classes. The remaining columns are features of the observation.
# Returns a list of mixture proportions, indexed by feature name.

#Note: This function calls the EM Algorithm function.

#Inputs:  A data set.
#Outputs: A list containing one element for all features in the input data
#    	  set.  Each list element contains a vector of k estimated mixing proportions.  

#----------------------------------------------------------------
# Roxy package build comments:
#' featureMixtureProportion(df)
#'
#' This function calls the \code{\link{emCalcMix}} function repeatedly, for each 
#' feature (column) in the data set given. Creates estimated class proportion vector 
#' for each feature. Data set must have 'class' as the first column.
#'
#' @param df A data frame containing rows of observations.  The first column must be
#' class.  Other columns contain features. Defaults to NULL.
#'
#' @keywords EM expectation-maximization
#'
#' @return A list containing one element for each feature in the data set.
#' @return Each list element is a vector of k mixing proportions.
#'
#' @examples
#' est_mixing_proportions <- featureMixtureProportion(df=unknown)
#'
#' @author Jennifer Starling
#'
#' @export

#----------------------------------------------------------------

featureMixtureProportion = function(df) {

  feature_names <- colnames(df)[2:ncol(df)]  # Assume Class is the first column
  
  p_list=list()  # Set up a list to hold the mixing proportions for each feature.
  
  #Extract all individual classes, in alphabetical order.
  classes <- sort(unique(df$class))
  k <- length(classes) #Number of classes.
  
  #Set up lists to hold kd and density functions.
  f_class <- list()
  kd <- list()
  densities <- list()
	
  # Iterate over the feature set
  for (f_name in feature_names) {
  
 	# Set up empirical density function for Classes 1-k for unknown data set.
	for (h in 1:k){
		f_class[[h]] = df[df$class==classes[h],f_name]
		kd[[h]] <- density(f_class[[h]])
		
		densities[[h]] = function(x,h){
			min_den <- quantile(kd[[h]]$y,.05)
			de <- approx(kd[[h]]$x,kd[[h]]$y,x)$y
			de[is.na(de)] <- min_den
			return(de)
			}
	} #End loop for classes.

	# Combine samples
 	f_sample = df[,f_name]

	# Estimate the mixing proportion
	em <- emCalcMix(f_sample,densities,p=rep(.2,5),maxiter=1000,conv=.00001)
	p_list[[f_name]] <- em$p
  }
 return(p_list)
}