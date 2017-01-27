# Calculate PUBMED figures

### Constants 
project_path <- "/Users/mansmagnusson/Dropbox (Personlig)/Doktorandstudier/Forskning/PartiallyCollapsedLDA-paper/"
results_path <- "results/Runs/stochvb_experiment/"
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
parse_nytwiki_paths_to_dataframe <- function(paths){
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
  df$seed <- unlist(lapply(stringr::str_split(df$txt, pattern = "_"), FUN=function(X) X[9]))

  # start run
  df$start_run <- lubridate::ymd_hms(substr(unlist(lapply(temp_list, FUN=function(X) X[13])), 4, 23))

  return(df)
}


pubmed_plt <- function(x,  k, dataset, seed=NULL ,it=c(0,5000), time_lim=100){
  df <- parse_nytwiki_paths_to_dataframe(paths = x$paths)
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
  plot_df
  plot_df <- plot_df[plot_df$runtime < time_lim, ]
  
  plt <- ggplot(data = plot_df[plot_df$iter <= it[2] & plot_df$iter >= it[1], ]) + 
    aes(x=runtime, y=loglik, group=group, color=algo) +
    geom_line()
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

param_df <- parse_nytwiki_paths_to_dataframe(paths = res_lik$paths)

dir.create(output_folder)
all_plots <- list()
time_limits <- c(nyt=1, wiki=0.25)
for(dat in c("nyt", "wiki")){ # dat <- "nyt"
  for(k in c(100)){ # k <- 100
    file_name <- paste0(paste("nytwiki", dat, k, sep="-"),".png")
    all_plots[[length(all_plots)+1]] <- plt <- pubmed_plt(x = res_lik, k=k, dataset=dat, it = c(10, 1000), time_lim = time_limits[dat]) + ylab("Log-likelihood") + xlab("Runtime (in hours)") + ggtitle(paste(dat, k, sep=" - ")) + labs(color="") 
    names(all_plots)[length(all_plots)] <- file_name
    ggsave(plot = plt  + theme_bw() + ggtitle(""), filename = paste0(output_folder, file_name), width = 6, height = 4)
  }
}
save(all_plots, file = paste0(output_folder, "nytwiki_plots.Rdata"))


