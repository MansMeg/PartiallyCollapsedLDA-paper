#' Function to do checks on the Phi matrix
#' 
#' @param phi_mat A Phi matrix
#' 
check_phi <- function(phi_mat){
  if(!all(rowSums(phi_mat) < 1)) warning("Phi rows sums to 1 (or more)")
  #  Not actual since the phi matrix is ordered once at iteration 10000
  #  for (i in 2:ncol(phi_mat)) {
  #    if(!all(phi_mat[,i-1] > phi_mat[, i])) warning("Phis not ordered")
  #  }
  return(0)
}

#' Function to do checks on the Theta matrix
#' 
#' @param theta_mat A theta matrix
#' 
check_theta <- function(theta_mat){
  if(!all(rowSums(theta_mat) < 1.0001 & rowSums(theta_mat) > 0.9999)) warning("Theta do not rows sum to 1")
  return(0)
}