# Functions
parse_vs_paths_to_dataframe <- function(paths){ # paths <- res_lik$paths
  temp_list <- stringr::str_split(string = paths, pattern = "/")
  df <- data.frame(no=1:length(temp_list),
                   txt=unlist(lapply(temp_list, FUN=function(X) X[length(X)-1])))
  # data_full
  tmp <- stringr::str_split(df$txt, pattern = "random_|_")
  df$data_full <- unlist(lapply(tmp, FUN=function(X) X[2]))
  
  # K
  df$k <- unlist(lapply(tmp, FUN=function(X) X[4]))
  df$pi <- unlist(lapply(tmp, FUN=function(X) X[6]))
  # Seed
  df$seed <- unlist(lapply(tmp, FUN=get_pos, text="seed"))
  # priors  
  return(df)
}

get_pos <- function(vec, text, pos=1){
  vec[which(str_detect(string = vec, pattern = text)) + pos]
}

full_plt_vs <- function(x, pi, seed=NULL, it =c(10,5000), y){
  df <- parse_vs_paths_to_dataframe(paths = x$paths)
  df$setup <- paste0(df$data_full, " (",df$k,")")
  part_df <- df[df$pi == pi,]
  if(!is.null(seed))  part_df <- part_df[part_df$seed == seed,]
  index <- part_df$no
  
  for(i in seq_along(index)){ # i <- 1
    plot_df_tmp <-x[[1]][[index[i]]]
    plot_df_tmp$setup <- part_df[i, "setup"]
    plot_df_tmp$seed <- paste(part_df[i, "seed"], part_df[i, "setup"])
    if(i == 1) {plot_df <- plot_df_tmp} else {plot_df <- rbind(plot_df,plot_df_tmp)}
  }

  plt <- ggplot(data = plot_df[plot_df$iter <= it[2] & plot_df$iter >= it[1], ]) + 
    aes(x=iter, color=setup, group=seed) + aes_string(y=y) +
    geom_line()
  return(plt)
}

lik_plt_vs <- function(x, data_set, k, seed=NULL, it =c(100,5000)){
  df <- parse_vs_paths_to_dataframe(paths = x$paths)

  part_df <- df[df$data_full == data_set & df$k == k, ]
  if(!is.null(seed))  part_df <- part_df[part_df$seed == seed,]
  index <- part_df$no
  
  for(i in seq_along(index)){ # i <- 1
    plot_df_tmp <-x[[1]][[index[i]]]
    plot_df_tmp$pi <- part_df[i, "pi"]
    plot_df_tmp$seed <- paste(part_df[i, "seed"], part_df[i, "pi"])
    if(i == 1) {plot_df <- plot_df_tmp} else {plot_df <- rbind(plot_df,plot_df_tmp)}
  }

  plt <- ggplot(data = plot_df[plot_df$iter <= it[2] & plot_df$iter >= it[1], ]) + 
    aes(x=iter, color=pi, group=seed, y=loglik) +
    geom_line()
  return(plt)
}


speed_vs_plt <- function(x, data_set, k, seed=NULL, it=NULL){
  times <- c("zTotalTime", "phiTotalTime")
  df <- parse_vs_paths_to_dataframe(paths = x$paths)
  
  part_df <- part_df_full <- df[df$data == data_set & df$k == k,]
  if(!is.null(seed))  part_df <- part_df[part_df$seed == seed,]
  index <- part_df$no
  part_df_full[,times] <- 0

  for(i in seq_along(index)){ # i <- 1
    plot_df_tmp <- x[[1]][[index[i]]]
    if(is.null(it)) it_tmp <- c(nrow(plot_df_tmp)-20, nrow(plot_df_tmp)) else it_tmp <- it
    part_df_full[i,times] <- colMeans(plot_df_tmp[it_tmp[1]:it_tmp[2], times])
  }
  
  part_df_z <- part_df
  part_df_z$sampling <- "z"
  part_df_z$time <- part_df_full$zTotalTime
  
  part_df_phi <- part_df
  part_df_phi$sampling <- "phi"
  part_df_phi$time <- part_df_full$phiTotalTime
  
  part_df_plot <- rbind(part_df_phi,part_df_z)
  part_df_plot <- aggregate(part_df_plot$time, by = list(part_df_plot$pi, part_df_plot$sampling), FUN=mean, na.rm=TRUE)
  names(part_df_plot) <- c("pi", "sampling", "time")
  part_df_plot <- part_df_plot[order(part_df_plot$sampling, decreasing = TRUE),]
  #part_df_plot$sampling <- factor(part_df_plot$sampling,levels = c("z","phi"))
  
  plt <- ggplot(data = part_df_plot) + 
    aes(x=pi, fill=sampling, y=time) +
    geom_bar(stat = "identity")
  return(plt)
}
