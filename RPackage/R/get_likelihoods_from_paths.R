#' Function to get likelihoods from file paths
#' 
#' @description
#'   Return list with loglikelihood calculations
#'  
#' @param paths
#'   Vector with paths to likelihood files
#' @param head
#'   Variable names in file - if null header is in file
#'
#' @examples
#' \dontrun{
#'   path <- "/Users/manma97/Desktop/Runs/"
#'   get_config_from_dir_recursive(path)
#' }
#' @export 
get_likelihoods_from_paths <- function(paths, head = c("iter", "loglik", "timestamp")) {
  logliks <- list()
  for (i in seq_along(paths)){ # i <- 1
    header <- is.null(head)
    temp_lik <- read.table(file = paths[i], header = header)
    if(!header) colnames(temp_lik) <- head[1:ncol(temp_lik)]
    timest_logical <- grepl(pattern = "timestamp", colnames(temp_lik))
    if(any(timest_logical)){
      for (j in which(timest_logical)){
        temp_lik[,j] <- as.POSIXct(temp_lik[,j]/1000, origin="1970-01-01")
      }
    }
    logliks[[i]] <- temp_lik
  }
  return(list(logliks=logliks, paths=paths))
}