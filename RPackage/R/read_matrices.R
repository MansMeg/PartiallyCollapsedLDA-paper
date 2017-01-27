#' Function to read in parameter values from binary matricies
#' 
#' @description
#'   Read in values from \code{first} element to \code{last} element in all
#'   binary files specified by \code{filenames}.
#'  
#' @param filenames  
#'   Vector with filenames to binary files to read.
#' @param rows
#'   The number of rows in the matrices.
#' @param cols
#'   The number of columns in the matrices.
#' @param type
#'   Whate type is the binary file ('integer' and 'numeric' is implemented)
#'
#' @return
#'   A list of matrices.
#'   
#' @export
read_matrices <- function(filenames, rows, cols, type=c("integer", "numeric")) {
  stopifnot(file.exists(filenames))
  stopifnot(type %in% c("integer", "numeric"),
            length(type) == 1)
  
  mat_list <- lapply(filenames, read_bin_matrix_file, type = "numeric", rows = rows, cols = cols)
  mat_list
}

read_bin_matrix_file <- function(filename, type, rows, cols){
  fileConn <- file(filename, "rb")
  if(type[1] == "integer"){
    read_values <- readBin(fileConn, integer(), n = rows * cols, 
                           endian = "big" )
    value <- integer(1)
  } else if(type[1] == "numeric"){
    read_values <- readBin(fileConn, double(), n = rows * cols, 
                           endian = "big" )
    value <- numeric(1)
  }
  close(fileConn)
  mat <- matrix(read_values, ncol = cols, byrow = TRUE)
  mat
}
