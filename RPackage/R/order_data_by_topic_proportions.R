#' Sort corpus by topic proportions
#' 
#' @description
#'  Sort corpus in order of topics proportions (in order by topics)
#'  
#' @param input_crp Path to corpus in Mallet format
#' @param output_sort_crp Path to output ordered file (by topics)
#' @param output_strat_crp Path to output ordered file (by topics) and then equally distributed
#' @param topic_prop Path to topic proportions output by PartiallyCollapse doc_topic_mean_filename config argument.
#'  
#' @export

order_data_by_topic_proportions <- function(input_crp, output_sort_crp, output_strat_crp, topic_prop, batches){
  crp <- readLines(input_crp)
  docno <- unlist(lapply(stringr::str_split(crp, ":|\t"), FUN=function(X) as.numeric(X[[2]])))
  tpc <- readLines(topic_prop)
  tmp_file <- tempfile()
  tmp <- stringr::str_replace_all(tpc, "([01])(,)([0-9])", "\\1.\\3")
  writeLines(tmp, con = tmp_file)
  rm(tpc, tmp)
  tpc <- read.csv(tmp_file, header = FALSE)
  stopifnot(nrow(tpc) == length(crp))
  rownames(tpc) <- docno
  topic_ord <- rev(do.call(order, tpc))
  sort_crp <- crp[topic_ord]
  writeLines(text = sort_crp, con = output_sort_crp)
  sort_tpc <- tpc[topic_ord,]
  sort_tpc$batch <- 1:batches
  strat_ord <- order(sort_tpc$batch)
  sort_tpc <- sort_tpc[strat_ord,]
  sort_crp <- sort_crp[strat_ord]
  writeLines(text = sort_crp, con = output_strat_crp)
}
  