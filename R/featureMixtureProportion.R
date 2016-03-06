#-------------------------------------------------------------
# Package: varstar
# Function: featureMixtureProportion(known,unknown)

#Purpose: Estimate mixing proportions for each feature of an unknown
#	data set.  (Calls emCalcMix function for each feature.) Unlike
#   featureMixtureProportion (original version), also returns convergence info.

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
#' featureMixtureProportion(unknown,known)
#'
#' This function calls the \code{\link{emCalcMix}} function repeatedly, for each 
#' feature (column) in the data set given. Creates estimated class proportion vector 
#' for each feature. Data set must have 'class' as the first column.
#'
#' @param known A data frame containing rows of observations, with known classes.  
#' The first column must be class.  Other columns contain features. Defaults to NULL.
#' @param unknown A data frame containing rows of observations, with unknown classes.  
#' Columns contain features. Defaults to NULL.
#' @param p A vector containing initial class proportion estimates. Defaults to (.2,.2,.2,.2,.2).
#'
#' @keywords EM expectation-maximization
#'
#' @return A list containing two objects: $phat and $convergence
#' @return $phat is a list containing one element for each feature in the data set.
#' @return A list containing one element for each feature in the data set.
#' @return Each $phat list element is a vector of k mixing proportions.
#' @return $convergence is a data frame containing details on the EM algorithm convergence.
#'
#' @examples
#' est_mixing_proportions <- featureMixtureProportion(unknown=df1,known=df2)
#'
#' @author Jennifer Starling
#'
#' @export

#----------------------------------------------------------------

featureMixtureProportion = function(unknown,known,p=rep(.2,5),maxiter=1000) { 
#Submit data set with unknown (df) & known classes, and starting vector of estimated proportions.
  maxiter = maxiter #Specify max number of iterations for the EM algorithm.
  p=p
  df = unknown	
  feature_names <- colnames(df)[2:ncol(df)]  # Assume class is the first column
  
  x <- list() #Set up a list to hold all of the lists below.
  p_list=list()  # Set up a list to hold the mixing proportions for each feature.
  iter_list = list() #Set up a list to hold the number of iterations until convergence for each feature.
  logl_list = list() #Set up a list to hold the loglikelihood vectors for each feature.
  
  #Extract all individual classes from known data set, in alphabetical order.
  classes <- sort(unique(known$class)) 
  k <- length(classes) #Number of classes in known data set.
  
  #Set up lists to hold kd and density functions.
  f_class <- list()
  kd <- list()
  densities <- list()
	
  # Iterate over the feature set
  for (f_name in feature_names) {
  
 	# Set up empirical density function for Classes 1-k for known data set.
	for (h in 1:k){
		f_class[[h]] = known[known$class==classes[h],f_name]
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
	em <- emCalcMix(f_sample,densities,p=p,maxiter=maxiter,conv=.00001)
	
	#Extract list of proportion estimates by feature for output.
	p_list[[f_name]] <- em$p
	
	#Extract data frame of iteration/log-likelihood detail by feature for output.
	iter_list[[f_name]] <- em$iter
	logl_list[[f_name]] <- -tail(em$logl,n=1) #Return just the max log-likelihood for ea. feature.
	
  }

	convergence <- data.frame(
		feature = (feature_names), 
		iter = unlist(iter_list),
		maxiter = rep(maxiter,length(feature_names)),
		max_logl = unlist(logl_list)
		)

 #Combine all output arguments into a list.
 return(list(phat=p_list,convergence = convergence))
 
}