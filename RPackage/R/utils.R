#' Print LDA model result files
#' 
#' @param x search path to results file
#' 
#' @export
print_config <- function(x) {
  cat(paste(suppressWarnings(readLines(x)), collapse ="\n"), "\n\n")
}
