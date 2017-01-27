#' Read in text documents as VCorpus objects
#' 
#' @param txt_file Text in "our" format to read in 
#' 
#' @examples
#' \dontrun{
#'  nips_corpus <- tm_reader("PCLDA_Data/nips.txt")
#'  enron_corpus <- tm_reader("PCLDA_Data/enron.txt")
#' }
#' @export
tm_reader <- function(txt_file){
  text <- readLines(txt_file)
  text <- unlist(lapply(strsplit(text, "\t"), function(X) X[3]))
  vcorp <- VCorpus(VectorSource(text))
  vcorp
}

