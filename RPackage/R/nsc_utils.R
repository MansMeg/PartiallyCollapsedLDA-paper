#' Convert numeric number of hours to NSC format
#'
#' @details 
#' Round up to closest 15 minutes
#'
#' @param h_num 
#'
#' @examples 
#' num_time_to_nsc_format(0.1)
#' num_time_to_nsc_format(9.99)
#' num_time_to_nsc_format(23.45)
num_time_to_nsc_format <- function(h_num){
  stopifnot(length(h_num) == 1,
            is.numeric(h_num),
            h_num > 0,
            h_num < 100)
  hh <- as.integer(h_num)
  mm <- as.integer(ceiling((h_num - hh) * 4))
  if(mm == 4){
    hh <- hh + 1
    mm <- 0
  }
  mm <- mm * 15
  hh_char <- ifelse(hh < 10, paste0("0", as.character(hh)), as.character(hh))
  mm_char <- ifelse(mm < 10, paste0("0", as.character(mm)), as.character(mm))
  paste0(hh_char, ":", mm_char, ":00")
}