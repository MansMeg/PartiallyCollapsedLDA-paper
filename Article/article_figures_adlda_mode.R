# Functions
parse_adlda_paths_to_dataframe <- function(paths){
  temp_list <- stringr::str_split(string = paths, pattern = "/")
  df <- data.frame(no=1:length(temp_list),
                   txt=unlist(lapply(temp_list, FUN=function(X) X[length(X)-1])))
  # data_full
  tmp <- stringr::str_split(df$txt, pattern = "mode_|_")
  df$data_full <- unlist(lapply(tmp, FUN=function(X) X[3]))
  
  # K
  df$k <- unlist(lapply(tmp, FUN=get_pos, text="cores", pos=-1))
  # Cores
  df$cores <- unlist(lapply(tmp, FUN=get_pos, text="cores"))
  # Seed
  df$seed <- unlist(lapply(tmp, FUN=get_pos, text="seed"))
  # Algo
  df$algo <- unlist(lapply(tmp, FUN=function(X) X[4]))
  
  return(df)
}

get_pos <- function(vec, text, pos=1){
  vec[which(str_detect(string = vec, pattern = text)) + pos]
}

full_core_plt <- function(x, data_set, k, algo, seed=NULL, it=c(100,5000), y = "loglik"){
  df <- parse_adlda_paths_to_dataframe(paths = x$paths)
  
  part_df <- df[df$data == data_set & df$k == k & df$algo == algo,]
  if(!is.null(seed))  part_df <- part_df[part_df$seed == seed,]
  index <- part_df$no
  
  for(i in seq_along(index)){ # i <- 44
    plot_df_tmp <- x[[1]][[index[i]]]
    plot_df_tmp$cores <- part_df[i, "cores"]
    plot_df_tmp$seed <- paste(part_df[i, "seed"], part_df[i, "cores"])
    if(i == 1) {plot_df <- plot_df_tmp} else {plot_df <- rbind(plot_df,plot_df_tmp)}
  }

  plt <- ggplot(data = plot_df[plot_df$iter <= it[2] & plot_df$iter >= it[1], ]) + 
    aes(x=iter, color=cores, linetype=cores, group=seed) + aes_string(y=y) +
    geom_line()
  return(plt)
}

find_error <- function(x, FUN, data_set=NULL, algo=NULL, k=NULL, cores=NULL){
  df <- parse_adlda_paths_to_dataframe(paths = x$paths)
  find_idx <- rep(TRUE,nrow(df))
  if(!is.null(data_set)) find_idx <- df$data == data_set & find_idx
  if(!is.null(algo)) find_idx <- df$algo == algo & find_idx
  if(!is.null(k)) find_idx <- df$k == k & find_idx
  if(!is.null(cores)) find_idx <- df$cores == cores & find_idx

  ids <- unlist(lapply(X = x[[1]],FUN = FUN))
  full_idx <- ids & find_idx
  cbind(df[full_idx,1:2], path = x$paths[full_idx])
}

#'Function that remove oldest runs in case of duplicates
#' 
remove_result_duplicates_adlda <- function(x){
  timedate_str <- str_extract(string = x[[2]], pattern = "[0-9]{4,4}-[0-9]{2,2}-[0-9]{2,2}--[0-9]{2,2}_[0-9]{2,2}_[0-9]{2,2}")
  idx <- order(timedate_str)
  x[[1]] <- x[[1]][idx]
  x[[2]] <- x[[2]][idx]
  df <- parse_adlda_paths_to_dataframe(paths = x$paths)
  correct_idx <- !duplicated(df$txt,fromLast = TRUE)
  x[[1]] <- x[[1]][correct_idx]
  x[[2]] <- x[[2]][correct_idx]
  x
}



comp_core_plt <- function(x, data_set, k, algo, seed=NULL, y = "loglik"){
  df <- parse_adlda_paths_to_dataframe(paths = x$paths)
  
  part_df <- df[df$data == data_set & df$k == k,]
  if(!is.null(seed))  part_df <- part_df[part_df$seed == seed,]
  index <- part_df$no
  
  for(i in seq_along(index)){ # i <- 1
    plot_df_tmp <- x[[1]][[index[i]]][nrow(x[[1]][[index[i]]])-1,]
    plot_df_tmp$cores <- as.numeric(part_df[i, "cores"])
    plot_df_tmp$algo <- part_df[i, "algo"]
    plot_df_tmp$seed <- paste(part_df[i, "seed"], part_df[i, "algo"])
    if(i == 1) {plot_df <- plot_df_tmp} else {plot_df <- rbind(plot_df,plot_df_tmp)}
  }
  
  plt <- ggplot(data = plot_df) + 
    aes(x=cores, color=algo, linetype=algo, group=seed) + aes_string(y=y) +
    geom_line()
  return(plt)
}


