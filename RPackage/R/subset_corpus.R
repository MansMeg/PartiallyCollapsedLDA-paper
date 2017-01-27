#' Function to randomly reorder documents in corpus file
#' 
#' @description
#'  Function that reads subsets a corpus (chooses the first part, so use a ranom ordered corpus)
#'  
#' @param input_file File to reorder
#' @param output_file Reordered file
#' @param prop Proportion of the full corpus
#' @param n Number of observations
#'  
#' @examples
#'   input_file <- "datasets/enron.txt" # 39860
#'   output_file <- "datasets/enron_10.txt" 
#'   subset_corpus(input_file = input_file, output_file = output_file)
#' @export
subset_corpus <- function(input_file, output_file, prop=0.1, n=NULL){
  file_in <- file(input_file, "r")
  if(is.null(n)){
    no_lines <- 10000L
    batch_size <- no_lines
    crp_size <- 0L
    while(batch_size == no_lines){
      crp <- readLines(file_in, n = no_lines)
      batch_size <- length(crp)
      crp_size <- crp_size + batch_size
    }
    n <- round(crp_size*prop)
  }
  close(file_in)  
  crp <- readLines(con = input_file, n = n)
  # Write output file
  file_out <- file(output_file, "w")
  writeLines(text = crp, con = file_out)
  close(file_out)
}

