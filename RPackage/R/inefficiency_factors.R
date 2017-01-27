#' Calculate inefficiency factors from list of matrices
#' 
#' @param mat_list A list of matrices
#' @param in_parallel Should the calculation use multiple cores. Default is FALSE.
#' 
#' 
#' @return A matrix of the same size with inefficiency factors per parameter
#' 
#' @export
inefficiency_factors <- function(mat_list, in_parallel=FALSE) {
  params <- matrix(unlist(lapply(mat_list, as.vector)), nrow = length(mat_list), byrow = TRUE)
  params <- lapply(seq_len(ncol(params)), function(i) params[,i])
  if(in_parallel){
    no_cores <- parallel::detectCores()
    params <- parallel::mclapply(params, inefficiency_factor, mc.cores = no_cores)
  } else {
    params <- lapply(params, inefficiency_factor)
  }
   matrix(unlist(params), ncol = ncol(mat_list[[1]]), byrow = TRUE)
} 

#' Inefficiency factor (v1)
#' 
#' @param X chain to calculate inefficiency for.
#' 
#' @export
inefficiency_factor <- function(X) {
  # X is the MCMC chain to calculate the inefficiency factor
  length(X) / (coda::effectiveSize(mcmc(data = X, start = 1, end = length(X))))
}


#' Inefficiency factor (v2)
#' 
#' @details
#'   Implementation of tau_a 
#' 
#' @param X chain to calculate inefficiency for.
#' 
#' @references
#' http://stats.stackexchange.com/questions/66369/definition-of-autocorrelation-time-for-effective-sample-size
#' 
#' @export
inefficiency_factor_v2 <- function(X, lag.max) {
  acf_values <- sum(as.vector(acf(X, lag.max = lag.max, type = "correlation", plot = FALSE)$acf)[-1])
  1 + 2 * acf_values
}

