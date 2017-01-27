# RS prop functions

parse_rs_prop_paths_to_dataframe <- function(paths){
  temp_list <- stringr::str_split(string = paths, pattern = "/")
  df <- data.frame(no=1:length(temp_list),
                   txt=unlist(lapply(temp_list, FUN=function(X) X[length(X)-1])))
  # data_full
  tmp <- stringr::str_split(df$txt, pattern = "_")
  df$data_full <- unlist(lapply(tmp, FUN=function(X) X[3]))
  
  # k
  df$k <- unlist(lapply(tmp, FUN=function(X) X[5]))
  df$step <- unlist(lapply(tmp, FUN=function(X) X[7]))
  df$seed <- unlist(lapply(tmp, FUN=function(X) X[9]))
  return(df)
}

get_pos <- function(vec, text, pos=1){
  vec[which(str_detect(string = vec, pattern = text)) + pos]
}

rs_prop_plt <- function(x, data_set, k, seed=NULL ,it=c(100,5000)){
  df <- parse_rs_prop_paths_to_dataframe(paths = x$paths)

  part_df <- df[df$data == data_set & df$k == k,]
  if(!is.null(seed))  part_df <- part_df[part_df$seed == seed,]
  index <- part_df$no
  part_df$time <- NA
    
  for(i in seq_along(index)){ #i<-1
    plot_df_tmp <- x[[1]][[index[i]]]
    plot_df_tmp$skip_step <- part_df[i, "step"]
    plot_df_tmp$seed <- paste(part_df[i, "seed"], part_df[i, "step"])
    if(i == 1) {plot_df <- plot_df_tmp} else {plot_df <- rbind(plot_df,plot_df_tmp)}
  }
  plot_df$skip_step <- factor(plot_df$skip_step, 
                              levels = c("1", "100", "200","500","1000"))
  levels(plot_df$skip_step)[1] <- "None"
  plt <- ggplot(data = plot_df[plot_df$iter <= it[2] & plot_df$iter >= it[1], ]) + 
    aes(x=runtime, y=loglik, color=skip_step, group=seed) +
    geom_line()
  return(plt)
}