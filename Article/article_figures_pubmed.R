# RS prop functions

parse_pubmed_paths_to_dataframe <- function(paths){
  temp_list <- stringr::str_split(string = paths, pattern = "/")
  df <- data.frame(no=1:length(temp_list),
                   txt=unlist(lapply(temp_list, FUN=function(X) X[length(X)-1])))
  # data_full
  tmp <- stringr::str_split(df$txt, pattern = "_")
  df$data_full <- unlist(lapply(tmp, FUN=function(X) X[2]))
  
  # k
  df$k <- unlist(lapply(tmp, FUN=function(X) X[4]))
  df$algo <- unlist(lapply(tmp, FUN=function(X) X[5]))
  df$cores <- unlist(lapply(tmp, FUN=function(X) X[7]))
  df$seed <- unlist(lapply(tmp, FUN=function(X) X[9]))
  df$run_start <- unlist(lapply(temp_list, FUN=function(X) X[12]))
  df$type <- unlist(lapply(temp_list, FUN=function(X) X[10]))
  return(df)
}

get_pos <- function(vec, text, pos=1){
  vec[which(str_detect(string = vec, pattern = text)) + pos]
}

pubmed_plt <- function(x,  k, seed=NULL ,it=c(100,5000), time_lim=100){
  df <- parse_pubmed_paths_to_dataframe(paths = x$paths)
  df$algo[df$algo=="spalias"] <- "PC-LDA"
  df$algo[df$algo=="adlda"] <- "AD-LDA"
  
  df <- df[order(df$run_start),] # Order to remove earliest runs (without job stealing)
  df_no_dup <- df[!duplicated(df[,2:5], fromLast = TRUE),]
  rownames(df_no_dup) <- NULL
  
  part_df <- df_no_dup[df_no_dup$k == k,]
  if(!is.null(seed))  part_df <- part_df[part_df$seed == seed,]
  index <- part_df$no
  part_df$time <- NA
    
  for(i in seq_along(index)){ #i<-1
    plot_df_tmp <- x[[1]][[index[i]]][,c("iter", "loglik", "runtime","zTotalTime","phiTotalTime")]
    plot_df_tmp$algo <- part_df[i, "algo"]
    plot_df_tmp$cores <- part_df[i, "cores"]
    plot_df_tmp$group <- paste(part_df[i, "algo"], part_df[i, "cores"])
    if(i == 1) {plot_df <- plot_df_tmp} else {plot_df <- rbind(plot_df,plot_df_tmp)}
  } 

  plot_df <- plot_df[plot_df$runtime < time_lim, ]

  plt <- ggplot(data = plot_df[plot_df$iter <= it[2] & plot_df$iter >= it[1], ]) + 
    aes(x=runtime, y=loglik, color=cores, group=group, linetype=algo) +
    geom_line()
  return(plt)
}