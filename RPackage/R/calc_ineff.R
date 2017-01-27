#' Calculate inefficiency of theta and phi
#' 
#' @description
#'   Reads data and calculates inefficiencies for Theta and Phi
#'   
#' @param bin_dir Search path to directory with binary files.
#' @param settings list with settings to use (verbose, top_no_words, test_doc, alpha_prior, beta_prior)
#' 
#' @return
#'   A list with params, ineff_phi and ineff_theta
#'   
#' @export
calculate_inefficiency <- function(bin_dir, settings){
  stopifnot(names(settings) %in% 
              c("verbose", "alpha_prior", "beta_prior", "V", "K", "D", 
                "no_top_words", "no_top_docs", "RunSuite", "seed"))
  stopifnot(is.numeric(settings$K), is.numeric(settings$V), is.numeric(settings$D),
            is.numeric(settings$alpha_prior), is.numeric(settings$beta_prior), 
            is.numeric(settings$no_top_docs), is.numeric(settings$no_top_words), 
            is.numeric(settings$seed))
  stopifnot(is.logical(settings$verbose))
  stopifnot(is.character(settings$RunSuite))

  
  # Get parameters of interest 
  details <- get_experiment_details_from_path(bin_dir = bin_dir)
  bin_files <- dir(bin_dir, full.names = TRUE)
  
  # Read in Phi
  param_files <- bin_files[stringr::str_detect(bin_files, "Phi")]
  ## Read in matrices
  mat_list <- read_matrices(filenames = param_files, rows = settings$K, cols = settings$no_top_words, type = "numeric")
  ## Do checks of Phi
  all(lapply(mat_list, check_phi) == 0)
  ## Calculate inefficiency
  ineff_phi <- inefficiency_factors(mat_list = mat_list)
#  if(setting$save) save(mat_list, details, settings,  
#                        file = paste0(results_path, "/data/",paste(c("Phi", rev(unlist(details))), collapse = "_"), ".Rdata"))
  rm(mat_list)
  
  # Read in Theta
  param_files <- bin_files[stringr::str_detect(bin_files, "Theta")]
  ## Read in matrices
  mat_list <- read_matrices(filenames = param_files, rows = settings$no_top_docs, cols = settings$K, type = "numeric")    
  ## Do checks of Theta
  all(lapply(mat_list, check_theta) == 0)
  ## Calculate inefficiency
  ineff_theta <- inefficiency_factors(mat_list = mat_list)
  rm(mat_list)
  
  return(list(details = details, ifs_theta = ineff_theta, ifs_phi = ineff_phi))
}
