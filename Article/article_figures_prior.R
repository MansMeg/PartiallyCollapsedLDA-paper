# Functions
parse_prior_paths_to_dataframe <- function(paths){
  temp_list <- stringr::str_split(string = paths, pattern = "/")
  df <- data.frame(no=1:length(temp_list),
                   txt=unlist(lapply(temp_list, FUN=function(X) X[length(X)-1])))
  # data_full
  tmp <- stringr::str_split(df$txt, pattern = "random_|_")
  df$data_full <- unlist(lapply(tmp, FUN=function(X) X[2]))
  
  # K
  df$k <- unlist(lapply(tmp, FUN=function(X) X[4]))
  # Algo
  df$algo <- unlist(lapply(tmp, FUN=function(X) X[5]))
  # Seed
  df$seed <- unlist(lapply(tmp, FUN=get_pos, text="seed"))
  # priors
  df$a <- unlist(lapply(tmp, FUN=function(X) X[7]))
  df$b <- unlist(lapply(tmp, FUN=function(X) X[9]))
  
  return(df)
}

get_pos <- function(vec, text, pos=1){
  vec[which(str_detect(string = vec, pattern = text)) + pos]
}

speed_plt <- function(x, data_set, k, a, b, seed=NULL, it=NULL){
  times <- c("zTotalTime", "phiTotalTime")
  df <- parse_prior_paths_to_dataframe(paths = x$paths)
  
  part_df <- part_df_full <- df[df$data == data_set & df$k == k & df$a == a & df$b == b,]
  if(!is.null(seed))  part_df <- part_df[part_df$seed == seed,]
  index <- part_df$no
  part_df_full[,times] <- 0
  
  for(i in seq_along(index)){ # i <- 10
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
  part_df_plot <- aggregate(part_df_plot$time, by = list(part_df_plot$algo, part_df_plot$sampling), FUN=mean, na.rm=TRUE)
  names(part_df_plot) <- c("algo", "sampling", "time")
  part_df_plot <- part_df_plot[order(part_df_plot$sampling, decreasing = TRUE),]
  #part_df_plot$sampling <- factor(part_df_plot$sampling,levels = c("z","phi"))
  
  plt <- ggplot(data = part_df_plot) + 
    aes(x=algo, fill=sampling, y=time) +
    geom_bar(stat = "identity")
  return(plt)
}



speed_nips_plt <- function(x, data_set, k, seed=NULL, it=NULL){
  times <- c("zTotalTime", "phiTotalTime")
  df <- parse_prior_paths_to_dataframe(paths = x$paths)
  
  part_df <- part_df_full <- df[df$data_full == data_set & df$k == k,]
  if(!is.null(seed))  part_df <- part_df[part_df$seed == seed,]
  index <- part_df$no
  part_df_full[,times] <- 0
  
  for(i in seq_along(index)){ # i <- 10
    plot_df_tmp <- x[[1]][[index[i]]]
    if(is.null(it)) it_tmp <- c(nrow(plot_df_tmp)-20, nrow(plot_df_tmp)) else it_tmp <- it
    part_df_full[i,times] <- colMeans(plot_df_tmp[it_tmp[1]:it_tmp[2], times])
    part_df_full$setup[i] <- paste(part_df_full$a[i], part_df_full$b[i])
  }
  
  part_df_z <- part_df
  part_df_z$sampling <- "z"
  part_df_z$time <- part_df_full$zTotalTime
  part_df_z$setup <- part_df_full$setup
  
  part_df_phi <- part_df
  part_df_phi$sampling <- "phi"
  part_df_phi$time <- part_df_full$phiTotalTime
  part_df_phi$setup <- part_df_full$setup
  
  part_df_plot <- rbind(part_df_phi,part_df_z)
  part_df_plot <- aggregate(part_df_plot$time, by = list(part_df_plot$algo, part_df_plot$sampling, part_df_plot$setup), FUN=mean, na.rm=TRUE)
  names(part_df_plot) <- c("algo", "sampling", "setup", "time")
  part_df_plot <- part_df_plot[order(part_df_plot$sampling, decreasing = TRUE),]
  #part_df_plot$sampling <- factor(part_df_plot$sampling,levels = c("z","phi"))
  
  plt <- ggplot(data = part_df_plot) + 
    aes(algo, fill=sampling, y=time) +
    geom_bar(stat = "identity") + facet_grid(~ setup)
  return(plt)
}
