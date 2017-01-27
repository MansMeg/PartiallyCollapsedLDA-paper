#' Function to get stats from stats.txt file paths
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
get_stats_from_paths <- function(paths) {
  stats <- list()
  for (i in seq_along(paths)){ # i <- 1
    temp_stat <- read.table(file = paths[i], header = TRUE)
    timest_logical <- grepl(pattern = "timestamp", colnames(temp_stat))
    if(any(timest_logical)){
      for (j in which(timest_logical)){
        temp_stat[,j] <- as.POSIXct(temp_stat[,j]/1000, origin="1970-01-01")
      }
    }
    stats[[i]] <- temp_stat
  }
  return(list(stats=stats, paths=paths))
}