#' Function to get stats from stats.txt file and likelihood.txt paths
#' 
#' @description
#'   Return list with results (times etc)
#'  
#' @param lik_paths
#'   Likelihood paths
#' @param stat_paths
#'   Stat paths
#' @param all_cores
#'   Should all results be included (count update times and z update times)
#' @param timediff_unit
#'   If choosen (secs or mins) the timediff is calculated. Default it is not calculated (ie NULL).
#' 
#' @export 
get_results_from_paths <- function(lik_paths, stat_paths, all_cores=FALSE, runtime_unit=NULL) {

  lik_substr <- str_split(string = lik_paths, pattern = "/")
  stat_substr <- str_split(string = stat_paths, pattern = "/")
  
  # Run checks
  stopifnot(all(unlist(lapply(lik_substr, FUN=function(X) X[length(X)])) == "likelihood.txt"))
  stopifnot(all(unlist(lapply(stat_substr, FUN=function(X) X[length(X)])) == "stats.txt"))  
  lik_substr <- unlist(lapply(lik_substr, FUN=function(X) X[length(X)-1]))
  stat_substr <- unlist(lapply(stat_substr, FUN=function(X) X[length(X)-1]))
  stopifnot(all(lik_substr == stat_substr))

  lik <- get_likelihoods_from_paths(paths = lik_paths)
  stat <- get_stats_from_paths(paths = stat_paths) 
  
  results <- list()
  for(i in seq_along(lik[[1]])){
    temp_res <- merge(x = lik[[1]][[i]], y = stat[[1]][[i]], by.x = "iter", by.y = "iteration")
    if(!all_cores) {
      temp_res <- temp_res[,!str_detect(colnames(temp_res), "z\\.")]
      temp_res <- temp_res[,!str_detect(colnames(temp_res), "countUpdate\\.")]
    }
    if(!is.null(runtime_unit)){
      temp_res$runtime <- 
        as.numeric(temp_res$timestamp - temp_res$timestamp[1], units = runtime_unit)
    }
    results[[i]] <- temp_res
  }
  return(list(results=results, paths=lik_paths))
}