# Calculate PUBMED figures

### Constants 
project_path <- "/Users/mansmagnusson/Dropbox (Personlig)/Doktorandstudier/Forskning/PartiallyCollapsedLDA-paper/"
results_path <- "results/Runs/pubmed_algo_experiment/"

options(scipen = 7, digits = 3)
library(ggplot2)
library(tm)
library(gridExtra)
library(pcplda)
library(ggthemes)

# Time unit in graphs
time_unit <- "hours"


### Functions
parse_pubmed_paths_to_dataframe <- function(paths){
  temp_list <- stringr::str_split(string = paths, pattern = "/")
  df <- data.frame(no=1:length(temp_list),
                   txt=unlist(lapply(temp_list, FUN=function(X) X[length(X)-1])))
  # data_full
  tmp <- stringr::str_split(df$txt, pattern = "_")
  part_of_pubmed <- grepl(pattern = "p[0-9]*_", df$txt)  
  df$data_full <- unlist(lapply(tmp, FUN=function(X) X[2]))
  df$data_full[part_of_pubmed] <- paste(df$data_full, unlist(lapply(stringr::str_split(df$txt, pattern = "_|[0-9]{4}"), FUN=function(X) X[4])))[part_of_pubmed]
  
  # k
  df$k <- unlist(lapply(tmp, FUN=function(X) X[4]))

  # algorithm
  df$algo <- unlist(lapply(tmp, FUN=function(X) X[5]))
  
  # seed
  df$seed <- unlist(lapply(stringr::str_split(df$txt, pattern = "random|_|p[0-9]{2}"), FUN=function(X) X[4]))

  # start run
  df$start_run <- lubridate::ymd_hms(substr(unlist(lapply(temp_list, FUN=function(X) X[13])), 4, 23))

  return(df)
}


pubmed_plt <- function(x,  k, dataset, seed=NULL ,it=c(0,5000), time_lim=100){
  df <- parse_pubmed_paths_to_dataframe(paths = x$paths)
  df$algo[df$algo=="spalias"] <- "sparse-PC-LDA"
  df$algo[df$algo=="adlda"] <- "sparse-AD-LDA"
  df$algo[df$algo=="lightpclda"] <- "light-PC-LDA"
  df$algo[df$algo=="lightcollapsed"] <- "light-AD-LDA"
  df <- df[df$algo!="lightpcldaw2",]
  
  df <- df[order(df$start_run),] # Order to remove earliest runs (without job stealing)
  df_no_dup <- df[!duplicated(df[,2:5], fromLast = TRUE),]
  rownames(df_no_dup) <- NULL
  
  part_df <- df_no_dup[df_no_dup$k == k & df_no_dup$data_full == dataset,]
  if(!is.null(seed))  part_df <- part_df[part_df$seed == seed,]
  index <- part_df$no
  part_df$time <- NA
  
  for(i in seq_along(index)){ #i<-1
    plot_df_tmp <- x[[1]][[index[i]]][,c("iter", "loglik", "runtime","zTotalTime","phiTotalTime")]
    plot_df_tmp$algo <- part_df[i, "algo"]
    plot_df_tmp$group <- paste(part_df[i, "seed"], paste(part_df[i, "algo"]))
    if(i == 1) {plot_df <- plot_df_tmp} else {plot_df <- rbind(plot_df,plot_df_tmp)}
    print(x[[2]][[index[i]]])
  } 
  
  plot_df <- plot_df[plot_df$runtime < time_lim, ]
  
  plt <- ggplot(data = plot_df[plot_df$iter <= it[2] & plot_df$iter >= it[1], ]) + 
    aes(x=runtime, y=loglik, group=group, color=algo) + guides(color=guide_legend(title="Sampler")) + 
    geom_line() + xlab("Runtime in hours") + ylab("Log-Likelihood")
  return(plt)
}


### Create plots
# Identify relevant paths
file_names <- dir(paste0(project_path, results_path), recursive = TRUE)
lik_files <- file_names[stringr::str_detect(file_names, pattern = "likelihood.txt")]
stat_files <- file_names[stringr::str_detect(file_names, pattern = "stats.txt")]

# Calculate tables
res_lik <- x <- get_results_from_paths(lik_paths = paste0(project_path, results_path, lik_files), 
                                  runtime_unit = time_unit, 
                                  stat_paths = paste0(project_path, results_path, stat_files))

param_df <- parse_pubmed_paths_to_dataframe(paths = res_lik$paths)

tmp_folder <- paste0("tmp_plt_",Sys.Date(),"/")
dir.create(tmp_folder)

dats <- c("pubmed p10", "pubmed")
ks <- c(10, 100, 1000)
time_limits <- list(c(0.2,0.3,1, 2), c(2,3,10, 20))
all_plots <- list()
for(dat in seq_along(dats)){ # dat <- 1
  for(k in seq_along(ks)){ # k <- 3
    file_name <- paste0(tmp_folder, paste("pubmed_algo", make.names(dats[dat]), ks[k], sep="-"),".png")
    all_plots[[length(all_plots)+1]] <- plt <- pubmed_plt(x = res_lik, k=ks[k], dataset=dats[dat], it = c(0, 1000), time = time_limits[[dat]][k])
    names(all_plots)[length(all_plots)] <- file_name
    ggsave(plot = plt + theme_bw() + ggtitle(""), filename = file_name, width = 6, height = 4)
  }
}

save(all_plots, file = paste0(tmp_folder, "pubmed_algo_plots.Rdata"))
