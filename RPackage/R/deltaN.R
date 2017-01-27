#' Function to read in deltaN values
#' 
#' @description
#'   Read in values from \code{first} element to \code{last} element in all
#'   binary files specified by \code{filenames}.
#'  
#' @param filename 
#'   Filename of a delta N file
#' @param first
#'   First element to read from the binary file.
#' @param last
#'   Last element to read from the binary file.
#'
#' @return
#'   A matrix containing the parameters from \code{first} to \code{last} and 
#'   with each files as a separate row.
#' 
#' @export
read_delta_N <- function(filename, first, last) {
  stopifnot(file.exists(filename))
  read_values <- as.vector(last - first + 1)
  fileConn <- file(filename, "rb")
  read_values <- readBin(fileConn, integer(), n = last, 
                         endian = "big" )[first:last]
  close(fileConn)  
  return(read_values)
}

#' Get the binary file element size for a given binary file with integers
#' 
#' @param filename File to get the number of elements from
#' 
#' @export
get_bin_size_ints <- function(filename){
  bin_elems <- as.numeric(floor(file.info(filename)[1] / 4)) # Ints are of size 4
  width <- ceiling(bin_elems/1000000)
  check_vect <- read_delta_N(filename = filename, 
                             first = bin_elems - width, 
                             last = bin_elems + width)
  diff <- min(which(is.na(check_vect))) - 2
  last_val <-  bin_elems - width + diff
  last_val
}

#' Function to read in delta file to one full delta Matrix (for the whole corpus)
#' 
#' @description
#'   The functions read in deltaN binary files into R and store them as list of spase matrices.
#'  
#' @param filename
#'   Filename of delta N file
#' @param D
#'   The number of documents to read in.
#' @param V
#'   The vocabulary size.
#'
#' @return
#'   A sparse matrices of size K * V.
#'   
#' @examples
#' \dontrun{
#'   filename <- "Runs/Run2014-11-12--10_31_02/delta_n/DeltaNs_noDocs_1499_vocab_12255_iter_1000.BINARY" 
#'   test <- read_delta_corpus_matrix(filename, D = 1499, V = 12255)
#' }
#' @export
read_delta_corpus_matrix <- function(filename, D, V){  
  
  next_elem <- 1
  doc <- integer(D)
  # Read in binary 
  int_vector <- read_delta_N(filename = filename, 1, get_bin_size_ints(filename))
  it <- int_vector[1]  
  K <- int_vector[3]
  full_mat <- matrix(0, nrow = K, ncol = V)

  for(d in 1:D){ # d<-1, D<-100
    if(d %% 100 == 0) message("Iteration: ", it, " : ", d) 
    doc_values <- int_vector[next_elem:(next_elem + 2)]
    doc[d] <- doc_values[2]
    next_elem <- next_elem + 3
    stopifnot(doc_values[1] == it, doc_values[3] == K)

    ctrl_sum <- 0
    for (k in 0:(K - 1)){ #k <- 0; k <- k + 1
      topic_len <- int_vector[next_elem : (next_elem + 1)]
      next_elem <- next_elem + 2
      stopifnot(k == topic_len[1])
      if(topic_len[2] == 0) next()
      res <- int_vector[next_elem : (next_elem + topic_len[2] * 2 - 1)]
      next_elem <- next_elem + length(res)

      # Add elements to matrix
      tokens_index <- 2 * ( 1 : ( length(res) / 2 ))
      full_mat[k + 1, res[tokens_index - 1] + 1] <- 
      full_mat[k + 1, res[tokens_index - 1] + 1] + res[tokens_index]
      ctrl_sum <- ctrl_sum + sum(res[tokens_index])
    }
    stopifnot(ctrl_sum == 0)
  }
  return(list(matrix=full_mat, K=K, iteration = it))
} 



#' Function analyze delta N per document
#' 
#' @description
#'   Apply an arbitrary function to a delta N matrix per document.
#'  
#' @param filename
#'   Filename of delta N file
#' @param D
#'   The number of documents to read in.
#' @param V
#'   The vocabulary size.
#' @param FUN
#'   Function to use to calculate for each Delta N.
#' @param ...
#'   Further arguments to send to FUN.
#'
#' @return
#'   A list with a list of results from function.
#'   
#' @examples
#' \dontrun{
#'   filename <- "Runs/Run2014-11-12--10_31_02/delta_n/DeltaNs_noDocs_1499_vocab_12255_iter_1000.BINARY" 
#'   test <- apply_delta_doc(filename, D = 100, V = 12255, FUN=function(X) sum(abs(X))/2)
#' }
#' @export
apply_delta_doc <- function(filename, D, V, FUN=sum, ...){  
  
  next_elem <- 1
  # Read in binary 
  doc <- integer(D)
  int_vector <- read_delta_N(filename = filename, 1, get_bin_size_ints(filename))
  K <- int_vector[3]
  it <- int_vector[1]
  doc_list <- list()
  
  for(d in 1:D){ # D <- 2
    if(d %% 500 == 0) message("Iteration: ", it, " Document: ", d) 
    doc_values <- int_vector[next_elem:(next_elem + 2)]
    doc[d] <- doc_values[2]
    next_elem <- next_elem + 3
    stopifnot(doc_values[1] == it, doc_values[3] == K)
    full_mat <- matrix(0, nrow = K, ncol = V)
    
    for (k in 0:(K - 1)){ #k <- 0; k <- k + 1
      topic_len <- int_vector[next_elem : (next_elem + 1)]
      next_elem <- next_elem + 2
      stopifnot(k == topic_len[1])
      if(topic_len[2] == 0) next()
      res <- int_vector[next_elem : (next_elem + topic_len[2] * 2 - 1)]
      next_elem <- next_elem + length(res)
      
      # Add elements to matrix
      tokens_index <- 2 * ( 1 : ( length(res) / 2 ))
      full_mat[k + 1, res[tokens_index - 1] + 1] <- 
        full_mat[k + 1, res[tokens_index - 1] + 1] + res[tokens_index]
    }        
    doc_list[[d]] <- FUN(full_mat, ...)
  }
  return(list(list=doc_list, K=K, iteration = it, doc_id = doc))
} 







