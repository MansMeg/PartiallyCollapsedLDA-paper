
options(scipen = 7, digits = 3)
library(ggplot2)
library(tm)
library(gridExtra)
library(pcplda)
library(ggthemes)


# Paths
project_path <- "/Users/mansmagnusson/Dropbox (Personlig)/Doktorandstudier/Forskning/PartiallyCollapsedLDA-paper/"
pos <- "results/prior/prior/"
project_path <- "/Users/mansmagnusson/Dropbox (Personlig)/Doktorandstudier/Forskning/PartiallyCollapsedLDA-paper/"

source(paste0(project_path, "Article/article_figures_prior.R"))

# Functions
prior_speed_plot <- function(x, data_set, k){
  times <- c("zTotalTime", "phiTotalTime")
  df <- parse_prior_paths_to_dataframe(paths = x$paths)
  
  part_df <- part_df_full <- df[df$data_full == data_set & df$k == k,]
  index <- part_df$no
  
  # Extract last 10 observations
  result_list <- list()
  for(i in seq_along(index)){ # i <- 1
    plot_df_tmp <- x[[1]][[index[i]]]
    it_tmp <- c(nrow(plot_df_tmp), nrow(plot_df_tmp))

    res <- suppressWarnings(cbind(part_df_full[i,], plot_df_tmp[it_tmp[1]:it_tmp[2], c("iter", times)]))
    res$setup <- paste(part_df_full$a[i], part_df_full$b[i])
    result_list[[i]] <- res
  }
  part_df <- do.call(rbind, result_list)
  
  part_df_z <- part_df
  part_df_z$sampling <- "z"
  part_df_z$time <- part_df_z$zTotalTime
  part_df_z$setup <- part_df_z$setup
  
  part_df_phi <- part_df
  part_df_phi$sampling <- "phi"
  part_df_phi$time <- part_df_z$phiTotalTime
  part_df_phi$setup <- part_df_z$setup
  
  part_df_plot <- rbind(part_df_phi,part_df_z)
  part_df_plot_stats <- aggregate(part_df_plot$time, by = list(part_df_plot$algo, part_df_plot$sampling, part_df_plot$setup), FUN=mean, na.rm=TRUE)
  names(part_df_plot_stats) <- c("algo", "sampling", "setup", "mean_time")
  part_df_plot_sd <- aggregate(part_df_plot$time, by = list(part_df_plot$algo, part_df_plot$sampling, part_df_plot$setup), FUN=sd, na.rm=TRUE)  
  part_df_plot_stats <- cbind(part_df_plot_stats, sd = part_df_plot_sd[, 4])
  part_df_plot_n <- aggregate(part_df_plot$time, by = list(part_df_plot$algo, part_df_plot$sampling, part_df_plot$setup), FUN=length)
  part_df_plot_stats <- cbind(part_df_plot_stats, obs = part_df_plot_n[, 4])
  part_df_plot_stats$se <- part_df_plot_stats$sd / sqrt(part_df_plot_stats$obs)
  
  part_df_plot_stats <- part_df_plot_stats[order(part_df_plot_stats$sampling, decreasing = TRUE),]
  #part_df_plot$sampling <- factor(part_df_plot$sampling,levels = c("z","phi"))
  
  part_df_plot_stats$ybegin <- part_df_plot_stats$mean_time - part_df_plot_stats$se
  part_df_plot_stats$yend <- part_df_plot_stats$mean_time + part_df_plot_stats$se
  part_df_plot_stats$ybegin[part_df_plot_stats$sampling == "phi"] <- part_df_plot_stats$ybegin[part_df_plot_stats$sampling == "phi"] + part_df_plot_stats$mean_time[part_df_plot_stats$sampling == "z"]
  part_df_plot_stats$yend[part_df_plot_stats$sampling == "phi"] <- part_df_plot_stats$yend[part_df_plot_stats$sampling == "phi"] + part_df_plot_stats$mean_time[part_df_plot_stats$sampling == "z"]
  part_df_plot_stats$ybegin[part_df_plot_stats$sampling == "phi" & part_df_plot_stats$algo == "adlda"] <- NA
  part_df_plot_stats$yend[part_df_plot_stats$sampling == "phi" & part_df_plot_stats$algo == "adlda"] <- NA  
  
  dodge <- position_dodge(width = 0.9)
  
  limits <- aes(ymax = yend, ymin=ybegin)
  
  plt <- ggplot(data = part_df_plot_stats) + 
    aes(algo, fill=sampling, y=mean_time) +
    geom_bar(stat = "identity") + facet_grid(~ setup) + 
    geom_errorbar(mapping = limits, width=0.25 , color = "darkgrey")
  return(plt)
}




# Read in data
file_names <- dir(paste0(project_path, pos), recursive = TRUE)
lik_files <- file_names[stringr::str_detect(file_names, pattern = "likelihood.txt")]
stat_files <- file_names[stringr::str_detect(file_names, pattern = "stats.txt")]

# Calculate tables
res_lik <- get_results_from_paths(lik_paths = paste0(project_path, pos, lik_files), runtime_unit = "mins",
                                  stat_paths = paste0(project_path, pos, stat_files))

# Compute base plots
prior_speed_plot_tmp <- prior_speed_plot(x = res_lik, data_set = "enron", k = "100")

# Cleanup plot
prior_speed_plot_tmp$data$algo[prior_speed_plot_tmp$data$algo=="adlda"] <- "AD-LDA"
prior_speed_plot_tmp$data$algo[prior_speed_plot_tmp$data$algo=="spalias"] <- "PC-LDA"

prior_speed_plot_tmp$data$setup[prior_speed_plot_tmp$data$setup=="001 001"] <- "list(alpha==0.01, beta==0.01)"
prior_speed_plot_tmp$data$setup[prior_speed_plot_tmp$data$setup=="01 01"] <- "list(alpha==0.1, beta==0.1)"
prior_speed_plot_tmp$data$setup[prior_speed_plot_tmp$data$setup=="001 01"] <- "list(alpha==0.01, beta==0.1)"
prior_speed_plot_tmp$data$setup[prior_speed_plot_tmp$data$setup=="01 001"] <- "list(alpha==0.1, beta==0.01)"

prior_speed_plot_tmp$data$setup <- factor(prior_speed_plot_tmp$data$setup)
#  levels(full_speed_plts[[i]]$data$setup) <- levels(full_speed_plts[[i]]$data$setup)[c(3,2,1,4)]

prior_speed_plot_tmp <- prior_speed_plot_tmp + theme_bw() + facet_grid(~ setup, labeller= label_parsed)  +
  xlab("Sampler") + ylab("Milliseconds per iteration") 


# Remove legend
prior_speed_plot_tmp <- prior_speed_plot_tmp + labs(fill = "Sampling of")+
  theme(legend.justification=c(1,0), legend.position=c(0.985,0.05)) +
  scale_fill_manual("Sampling",labels = c(z = expression(bold(z)), phi = expression(Phi)), values = c("black", "lightgray"))

tmp_folder <- paste0("tmp_plt_",Sys.Date(),"/")
dir.create(paste0(project_path,tmp_folder))

ggsave(filename = paste0(project_path, tmp_folder, "prior_speed.png"),
       prior_speed_plot_tmp, width = 6, height = 3.5)


