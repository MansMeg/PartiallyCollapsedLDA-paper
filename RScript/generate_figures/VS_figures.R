### Constants 
project_path <- "/Users/manma97/Dropbox/Doktorandstudier/Forskning/PCLDA/"
results_path <- "results_20160706/PCLDA/Runs/vs/"
output_folder <- paste0("tmp_plt_",Sys.Date(),"/")

options(scipen = 7, digits = 3)
library(ggplot2)
library(tm)
library(gridExtra)
library(pcplda)
library(ggthemes)

# Time unit in graphs
time_unit <- "hours"


### Functions
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

full_plt_vs <- function(x, pi, k=NULL, data_set=NULL, seed=NULL, it =c(10,5000), y){
  df <- parse_vs_paths_to_dataframe(paths = x$paths)
  df$setup <- paste0(df$data_full, " (",df$k,")")
  part_df <- df[df$pi == pi,]
  if(!is.null(seed))  part_df <- part_df[part_df$seed == seed,]
  if(!is.null(data_set))  part_df <- part_df[part_df$data_full == data_set,]
  if(!is.null(k))  part_df <- part_df[part_df$k == k,]
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


### Create plots
# Read in prop
#source(paste0(project_path, "Article/article_figures_vs.R"))
#pos <- "results/vs/vs/"

# Plot constants
file_names <- dir(paste0(project_path, results_path), recursive = TRUE)
lik_files <- file_names[stringr::str_detect(file_names, pattern = "likelihood.txt")]
stat_files <- file_names[stringr::str_detect(file_names, pattern = "stats.txt")]

data_sets <- c("nips", "enron", "pubmed")
ks <- c("20", "100")

y <- c("typeTokenDensity", "documentDensity", "phiDensity") # typeTokenDensity, documentDensity, loglik
y_ylab <- c("Wordtype-topic sparsity", "Document-topic sparsity", "Zero elements in Phi")
y_it <- list(c(10,5000), c(10,5000), c(10,3000))
pis <- c("05", "01", "001", "0001")


# Calculate tables
res_lik <- get_results_from_paths(lik_paths = paste0(project_path, results_path, lik_files), 
                                  runtime_unit = "mins",
                                  stat_paths = paste0(project_path, results_path, stat_files))
df <- parse_vs_paths_to_dataframe(paths = res_lik$paths)

dir.create(output_folder)
all_plots <- list()
all_results <- list()
for (pi in pis){ # pi <- pis[1]
  for(k in ks){ # k <- ks[1]
    full_vs_plts <- list() 
    for(i in seq_along(y)){ # i <- 1
      file_name <- paste0(paste("vs_plot", y[i], pi, k, sep = "-"),".png")
      all_plots[[length(all_plots)+1]] <- plt <- full_plt_vs(x = res_lik, pi=pi, k = k, y=y[i], it=y_it[[i]])+ xlab("Iteration") + ylab(y_ylab[i])  
      # all_results[[length(all_results)+1]] <- results_from_vs(x = res_lik, pi=pi, k = k, y=y[i], it=y_it[[i]])
      names(all_plots)[length(all_plots)] <- file_name
      ggsave(plot = plt + ggtitle(paste("pi =", pi, "  K =", k, collapse=" - ")), 
             filename = paste0(output_folder, file_name))
      }
    }
}

# Likelihood plots
for (data_set in data_sets){ # data_set <- "pubmed"
  lik_vs_plts <- list()
  for(i in seq_along(ks)){ # i <- 1
    file_name <- paste0(paste("vs_lik_plot", y[i], data_set, ks[i], sep = "-"),".png")
    lik_vs_plts[[i]] <- lik_plt_vs(x = res_lik, data_set = data_set, k = ks[i])
    all_plots[[length(all_plots)+1]] <- lik_vs_plts[[i]] <- lik_vs_plts[[i]]  + ggtitle(paste0(data_set," with ", ks[i]," topics")) + #my_theme() +
      xlab("Iteration") + ylab("Log-Likelihood") 
    names(all_plots)[length(all_plots)] <- file_name
    ggsave(plot = lik_vs_plts[[i]], 
           filename = paste0(output_folder, file_name))
  }
}


# Timing plots
for (data_set in data_sets){ # data_set
  time_vs_plts <- list()
  for(i in seq_along(ks)){ # i <- 1
    file_name <- paste0(paste("vs_time_plot", data_set, ks[i], sep = "-"),".png")
    time_vs_plts[[i]] <- speed_vs_plt(x = res_lik, data_set = data_set, k = ks[i])
#    time_vs_plts[[i]]$data$pi <- rep(c("0.01", "0.1", "0.5", "0.0001"), 2)
    all_plots[[length(all_plots)+1]] <- time_vs_plts[[i]] <- time_vs_plts[[i]] + ggtitle(paste0(data_set, ks[i]," topics")) +
      xlab(expression(pi)) + ylab("Sampling time")  
    ggsave(plot = time_vs_plts[[i]], 
           filename = paste0(output_folder, file_name))
    names(all_plots)[length(all_plots)] <- file_name
  }
}

save(all_plots, file = paste0(output_folder, "vs_plots.Rdata"))
