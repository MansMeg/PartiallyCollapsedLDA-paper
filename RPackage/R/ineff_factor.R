#' Function to read in parameter values
#' 
#' @description
#'   Read in values from \code{first} element to \code{last} element in all
#'   binary files specified by \code{filenames}.
#'  
#' @param filenames  
#'   Vector with filenames to binary files to read.
#' @param first
#'   First element to read from the binary file.
#' @param last
#'   Last element to read from the binary file.
#' @param type
#'   Whate type is the binary file ('integer' and 'numeric' is implemented)
#'
#' @return
#'   A matrix containing the parameters from \code{first} to \code{last} and 
#'   with each files as a separate row.
#'   
#' @export
read_part_param <- function(filenames, first, last, type=c("integer", "numeric")) {
  stopifnot(file.exists(filenames))
  if(type[1] == "integer"){
    value <- integer(1)
  } else if(type[1] == "numeric"){
    value <- numeric(1)
  } else {
    stop("Type not 'numeric' or 'integer'.")
  }
  
  params <- matrix(value, ncol=last - first + 1, length(filenames))
  read_values <- vector(mode = type,length = last - first + 1)
  
  for(i in seq_along(filenames)){ # debug: i <-1
    fileConn <- file(filenames[i], "rb")
    if(type[1] == "integer"){
      read_values <- readBin(fileConn, integer(), n = last, 
                           endian = "big" )[first:last]
    } else if(type[1] == "numeric"){
      read_values <- readBin(fileConn, double(), n = last, 
                             endian = "big" )[first:last]
    }
    close(fileConn)
    params[i,] <- read_values
  }
  
  class(params) <- c("param_matrix", "matrix") 
  return(params)
}



#' Function to calculate top words
#' 
#' @description
#'   Calculates the top words from a param_matrix based on mean value of phi_i
#'  
#' @param filenames
#'   filenames (vector) to use for calculations
#' @param K The number of topics
#' @param V The vocabulary size
#' @param top_no_words the number of top words in topics to use.
#' @param verbose Should the run be verbose?
#'   
#' @return
#'   A list of length K with matrices of size T * top_no_words, stored with iterations as rows.
#'   
#' @examples
#' \dontrun{
#'   topic_top_word_list <- 
#'     get_top_topics_param_mat(filenames, K=20, V = 12127, top_no_words = 5, verbose = TRUE)
#' }
#' @export
get_top_phi_param_mat <- function(filenames, K, V, top_no_words = NULL, verbose = TRUE){
  # Debug: K = 20; k <- 1; top_no_words = 5; verbose = TRUE
  topic_top_word_list <- lapply(rep(0.0,K),FUN = matrix, ncol=top_no_words, nrow=length(filenames))
  for(k in 1:K){
    if(verbose) cat("Topic:",k," (parameters", (k - 1) * V + 1, "to", k * V, ")\n") # Debug: k <- 1
    temp_topic <- read_part_param(filenames = filenames, first = (k - 1) * V + 1, last = k * V, type="numeric")
    if(!all(round(rowSums(temp_topic), 1) == 1.0)) warning("sum of a dirichlet >= 1.05.")
    if(!is.null(top_no_words)){
      sorted_index <- sort(colSums(temp_topic), index.return=TRUE, decreasing = TRUE)
      temp_topic <- temp_topic[,sorted_index$ix[1:top_no_words]]
    }
    topic_top_word_list[[k]] <- temp_topic
  }
  class(topic_top_word_list) <- c("PCLDA_param_list", "list")
  topic_top_word_list
}

#' @title
#' r_dirichlet
#' 
#' @description
#' Random draws from a dirichlet distribution
#' 
#' @param n number of draws.
#' @param alpha matrix or vector
#' 
#' @export
r_dirichlet <- function (n, alpha) 
{
  if(is.matrix(alpha)){
    k <- nrow(alpha)
    l <- ncol(alpha)
    alpha <- as.vector(t(alpha))
  } else if (is.vector(alpha)) {
    k <- 1
    l <- length(alpha)
  } 
  x <- matrix(rgamma(n = n * k * l, shape = alpha), ncol = l, byrow = TRUE)
  sm <- x %*% rep(1, l)
  return(x/as.vector(sm))
}

test_that("Test of dirichlet",code = {
  expect_true(object = all(round(r_dirichlet(1, matrix(1000000, ncol=3, nrow=2)),2) == 0.33))
  expect_true(object = all(round(rowSums(r_dirichlet(3, matrix(c(1,4,6,12,12), ncol=3, nrow=5))),4) == 1))  
})

#' Function to calculate theta for a subset of documents
#' 
#' @description
#'   Calculates theta from an M matrix.
#'   
#' @param filenames M binary files to use (search paths)
#' @param D the number of documents to use (top D documents)
#' @param K the number of topics
#' @param alpha_prior the alpha prior for M
#' @param verbose Should the calculations be verbose?

#' @return
#'   A list with matrices of same size, stored with iterations as rows.
#'   
#' @export
get_top_D_documents_param_mat <- function(filenames, D, K, alpha_prior, verbose = TRUE){
  # Debug: K = 20; d <- 1; D = 100; verbose = TRUE
  topic_document_list <- list()
  for(d in 1:D){
    if(verbose) cat("Document:", d, " (parameters", (d - 1) * K + 1, "to", d * K, ")\n")
    temp_doc <- read_part_param(filenames, first = (d - 1) * K + 1, last = d * K, type = "integer")
    # Draw dirichlet 
    temp_doc <- r_dirichlet(n=1, alpha = temp_doc + alpha_prior)
    topic_document_list[[d]] <- temp_doc
  }
  class(topic_document_list) <- c("PCLDA_param_list", "list")
  topic_document_list
}


#' Function to calculate phi from a N matrix
#' 
#' @description
#'   Calculates phi for top words from an N matrix.
#'
#' @inheritParams get_top_phi_param_mat
#' @param beta_prior the prior for phi
#'   
#' @return
#'   A list with matrices of same size, stored with iterations as rows.
#'   
#' @export
get_top_phi_from_N_param_mat <- function(filenames, K, V, beta_prior, top_no_words = NULL, verbose = TRUE){
  # Debug: K = 20; k <- 1; top_no_words = 5; verbose = TRUE
  topic_top_word_list <- list()
  for(k in 1:K){
    if(verbose) cat("Topic:",k," (parameters", (k - 1) * V + 1, "to", k * V, ")\n")
    temp_topic <- read_part_param(filenames, first = (k - 1) * V + 1, last = k * V, type = "integer")    
    temp_topic <- r_dirichlet(n=1,  alpha = temp_topic + beta_prior)    
    if(!is.null(top_no_words)){
      sorted_index <- sort(colSums(temp_topic), index.return=TRUE, decreasing = TRUE)
      temp_topic <- temp_topic[,sorted_index$ix[1:top_no_words]]
    }
    topic_top_word_list[[k]] <- temp_topic
  }
  class(topic_top_word_list) <- c("PCLDA_param_list", "list")
  topic_top_word_list
}



#' Calculate inefficiency factors (IF)
#' 
#' @description
#'   Calculates IF for all paramaters of an \code{PCLDA_param_list}
#'   object.
#'   
#' @details
#'   The autocovariance can be calculated in the following way:
#'   \deqn{\tau_{a}=1+2\cdot\sum_{k=1}^{\infty}\rho_{k}}
#'   
#' @param x object of class \code{PCLDA_param_list}
#' @param r length of sum (see details).
#' 
#' @examples
#' \dontrun{
#'   inefficiency_factors(x = x, r = 10)
#' }
#' @references
#'    \url{http://stats.stackexchange.com/questions/66369/definition-of-autocorrelation-time-for-effective-sample-size}
#' 
#' @return
#'   A list of vectors with IFs.
#'   
#' @export

inefficiency_factors <- function(x, r){
  stopifnot(inherits(x, "PCLDA_param_list"))

  res_list <- list(length(x))
  for(k in 1:length(x)){ 
    res_list[[k]] <- apply(X = x[[k]], MARGIN = 2, FUN = ineff_calc)
  }
  res_list
}

#' Inefficiency calculations
#' 
#' @param X chain to calculate inefficiency for.
#' 
#' @export
ineff_calc <- function(X) {
  # X is the MCMC chain to calculate the inefficiency factor for
  length(X) / (coda::effectiveSize(mcmc(data = X, start = 1, end = length(X))))
}
