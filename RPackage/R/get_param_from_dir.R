#' Function that get all parameters of interest from a binary directory
#' 
#' @description
#'   Compute the parameters of interest from binary files (in a binary directory)
#'  
#' @param bin_dir 
#'   Path to a directory with binary files calculated with tgs.runnables.InefficencyExperiment
#'
#' @return
#'   A list containing the parameters:
#'   experiment_no, V, K, D, algo_type, iterations, run_suite
#' 
#' @export
get_param_from_dir <- function(bin_dir) {
  param <- list()
  bin_files <- dir(bin_dir)
  bin_part <- stringr::str_split(bin_dir, "/")[[1]]
  bin_files_M <- bin_files[stringr::str_detect(bin_files, "M_")]
  bin_files_N <- bin_files[stringr::str_detect(bin_files, "N_")]

  param$experiment_no <- 
    as.numeric(stringr::str_split(bin_part[stringr::str_detect(bin_part, "experiment")], "experiment")[[1]][2])  
  param$algo_type <- 
    stringr::str_split(bin_part[stringr::str_detect(bin_part, "experiment")], "_experiment")[[1]][1]
  param$run_suite <- bin_part[stringr::str_detect(bin_part, "RunSuite")]
  param$run <- bin_part[stringr::str_detect(bin_part, "Run20")]
  switch(EXPR = param$algo_type,
         uncollapsed = i <- 0,
         collapsed = i <- 1,
         stop())
  param$V <- as.numeric(stringr::str_split(bin_files_N[1],"(_|\\.)")[[1]][2 + i])
  param$K <- as.numeric(stringr::str_split(bin_files_M[1],"(_|\\.)")[[1]][3 + i])
  param$D <- as.numeric(stringr::str_split(bin_files_M[1],"(_|\\.)")[[1]][2 + i])
  param$iters <- max(as.numeric(unlist(lapply(stringr::str_split(bin_files_M,"(_|\\.)"),
                                              function(X) X[[4 + i]]))))
  return(param)
}



#' Get parameters of interest for the actual experiment
#' 
#' @description
#'   Compute the parameters of interest from a given path
#'  
#' @param bin_dir 
#'   Path to a directory with binary files calculated with tgs.runnables.InefficencyExperiment
#'
#' @return
#'   A list containing the parameters:
#'   experiment_no, algo_type, iterations and run
#' 
#' @export
get_experiment_details_from_path <- function(bin_dir) {
  param <- list()
  bin_files <- dir(bin_dir)
  bin_part <- stringr::str_split(bin_dir, "/")[[1]]
  
  param$experiment_no <- 
    as.numeric(stringr::str_split(bin_part[stringr::str_detect(bin_part, "experiment")], "experiment")[[1]][2])  
  param$algo_type <- 
    stringr::str_split(bin_part[stringr::str_detect(bin_part, "experiment")], "_experiment")[[1]][1]
  param$run <- bin_part[stringr::str_detect(bin_part, "Run20")]
  return(param)
}




#' Function to return all configs from a folder with runsuites
#' 
#' @description
#'   Return the java setup files from any given run using tgs.runnables.InefficencyExperiment
#'  
#' @param path  
#'   Path to folder with runsuites
#'    
#' @examples
#' \dontrun{
#'   path <- "/Users/manma97/Desktop/Runs/"
#'   get_config_from_dir_recursive(path)
#' }
#' @export
get_config_from_dir_recursive <- function(path) {
  files <- dir(path, recursive = TRUE, full.names = TRUE)
  files <- files[!stringr::str_detect(files, "\\.BINARY")]
  conv_files <- unique(files[stringr::str_detect(files, "Convergence\\.txt")])
  cfg_files <- unique(files[stringr::str_detect(files, "cfg")])
#  conv_list <- suppressWarnings(lapply(conv_files, readLines))
#  cfg_list <- suppressWarnings(lapply(cfg_files, readLines))
  list(convergence_files = conv_files,
       cfg_files = cfg_files)
}


#' Function to parse cfg_configs
#' 
#' @param cfg_file The cfg_file to get the parameter from 
#' @param pattern The parameter that should be return as regexp
#' 
#' @export
get_param_from_config <- function(cfg_file, pattern){
  res <- suppressWarnings(readLines(cfg_file))
  res <- stringr::str_split(res[stringr::str_detect(res, pattern)], "[=#]")
  res <- stringr::str_trim(unlist(lapply(res, FUN=function(X) X[[2]])))
  nums <- !is.na(suppressWarnings(as.numeric(res)))
  res <- ifelse(nums, as.numeric(res), res)
  if(length(res) == 0) stop("not a valid parameter in config")
  res
}


#' Function to parse cfg_configs
#' 
#' @param conv_files The convergence files to get the parameter from 
#' @param pattern The parameter that should be return as regexp
#' 
#' @export
get_param_from_convergence <- function(conv_files, pattern){
  res <- suppressWarnings(lapply(conv_files, readLines))
  res <- unlist(lapply(res, FUN=function(X) stringr::str_split(X[stringr::str_detect(X, pattern)], "[:#]")))
  res <- stringr::str_trim(res[!stringr::str_detect(res, pattern)])
  nums <- !is.na(suppressWarnings(as.numeric(res)))
  res <- ifelse(nums, as.numeric(res), res)
  if(length(res) == 0) stop("not a valid parameter in config")
  res
}


#' Calculate the Runsuite based on the config
#' 
#' @details
#'   It checks the subconfig name from the convergence txt-file (Active subconfig)
#' 
#' @param project_path path to folder with all RunSuites
#' @param config The config to get the runsuite for (ex. "nips_10")
#' 
#' @export
get_runsuite_from_config <- function(project_path, config){
  cfgs <- get_config_from_dir_recursive(project_path)
  subconfigs <- 
    get_param_from_convergence(conv_files = cfgs$convergence_files, pattern = "subconfig")
  correct_path <- cfgs$convergence_files[subconfigs %in% config]
  res <- unlist(stringr::str_split(correct_path, "/"))
  res[stringr::str_detect(string = res, pattern = "RunSuite")]
}


#' Function to get likelihoods from files
#' 
#' @description
#'   Return list with loglikelihood calculations
#'  
#' @param path  
#'   Path to folder with runsuites
#'
#' @examples
#' \dontrun{
#'   path <- "/Users/manma97/Desktop/Runs/"
#'   get_config_from_dir_recursive(path)
#' }
#' @export 
get_likelihoods_from_dir <- function(path) {
  files <- dir(path, recursive = TRUE, full.names = TRUE)
  files <- files[!stringr::str_detect(files, "\\.BINARY")]
  likelihoods <- unique(files[stringr::str_detect(files, "likelihood\\.txt")])
  logliks <- list()
  for (i in seq_along(likelihoods)){
    temp_lik <- read.table(file = likelihoods[i], header = FALSE)
    colnames(temp_lik) <- c("iter", "loglik", "timestamp")
    temp_lik$timestamp <- as.POSIXct(temp_lik$timestamp/1000, origin="1970-01-01")
    logliks[[i]] <- temp_lik
  }
  names(logliks) <- likelihoods
  logliks
}


