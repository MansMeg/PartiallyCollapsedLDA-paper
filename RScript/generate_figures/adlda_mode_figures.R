
options(scipen = 7, digits = 3)
library(ggplot2)
library(tm)
library(gridExtra)
library(pcplda)
library(ggthemes)


project_path <- "/Users/mansmagnusson/Dropbox (Personlig)/Doktorandstudier/Forskning/PCLDA/"
pos <- "/results/rs_prop/RunsRS_prop/"
base_path <- "/Users/mansmagnusson/Dropbox (Personlig)/Doktorandstudier/Forskning/pclda"

tmp_folder <- paste0("tmp_plt_",Sys.Date(),"/")
dir.create(tmp_folder)

# Read in prop
source(paste0(project_path, "Article/article_figures_adlda_mode.R"))
# old: pos <- "results/adlda_mode/adlda_mode/"
pos <- "results/adlda_mode_2/"


# Set overall theme
my_theme <- theme_bw

# Seed
set.seed(20141104)

#General functions
get_rare_word_limit <- function(tokens, p = c(0.001, 0.005, 0.01, 0.02*1:5)){
  tokens <- sort(tokens, decreasing = FALSE)
  c_s <- cumsum(tokens)
  prop <- c_s / c_s[length(c_s)]
  
  vapply(X = 1:length(p), FUN.VALUE = double(1), FUN=function(x) max(tokens[prop <= p[x]]))
}


# Figures AD-LDA mode
file_names <- dir(paste0(project_path, pos), recursive = TRUE)
lik_files <- file_names[stringr::str_detect(file_names, pattern = "likelihood.txt")]
stat_files <- file_names[stringr::str_detect(file_names, pattern = "stats.txt")]

# Time unit in graphs
time_unit <- "mins"

# Calculate tables
res_lik <- get_results_from_paths(lik_paths = paste0(project_path, pos, lik_files), runtime_unit = "mins",
                                  stat_paths = paste0(project_path, pos, stat_files))
# Remove duplicates
res_lik <- remove_result_duplicates_adlda(res_lik)


# Remove erroneous 
#rem <- c(333,329,345,348,334,342,340,346)
#res_lik <-list(results=res_lik[[1]][-rem], paths=res_lik[[2]][-rem]) 
rem_bug <- c(346, 349, 332, 345, 352, 351)
# test <- parse_adlda_paths_to_dataframe(paths = res_lik$paths)
# test[which(test$data_full=="nips" & test$k=="20" & test$seed=="4720" & test$algo=="adlda"),"no"]
rem_all <- c(258,264,268,346,256,261,262,349,260,266,269,351,303,312,315,352,97,116,332,334,193,206,211,345)
res_lik <-list(results=res_lik[[1]][-rem_all], paths=res_lik[[2]][-rem_all]) 

# AD-LDA2 runs
# find_error(x = res_lik, data_set="enron", k="20", algo="adlda", FUN=function(X) any(X$loglik[10:length(X$loglik)] < -200000000))
# find_error(x = res_lik, data_set="nips", k="20", algo="adlda", FUN=function(X) any(X$loglik[10:length(X$loglik)] < -75000000))
# find_error(x = res_lik, cores = "8", data_set="enron", k="100", algo="adlda", FUN=function(X) any(X$documentDensity[length(X$documentDensity)] > 0.114))
# find_error(x = res_lik, cores = "8", data_set="enron", k="20", algo="adlda", FUN=function(X) any(X$documentDensity[length(X$documentDensity)] > 0.25))


# find_error(x = res_lik, FUN = function(X) any(X$loglik[length(X$loglik)] < -75000000))
# Likelihood errors
#     no                                          txt
#223 223  adlda_mode_nips_adlda_100_cores_8_seed_4713
#352 352 adlda_mode_enron_adlda_100_cores_8_seed_4721
# rem <- c(223,187)


# find_error(x = res_lik, FUN = function(X) any(length(X$loglik) < 400))
#      no                                          txt
# 187 187   adlda_mode_nips_adlda_20_cores_8_seed_4719
# 189 189   adlda_mode_nips_adlda_20_cores_8_seed_4720
# 190 190   adlda_mode_nips_adlda_20_cores_8_seed_4721
# 222 222  adlda_mode_nips_adlda_100_cores_8_seed_4712
# 223 223  adlda_mode_nips_adlda_100_cores_8_seed_4713
# 230 230  adlda_mode_nips_adlda_100_cores_8_seed_4717
# 233 233  adlda_mode_nips_adlda_100_cores_8_seed_4719
# 348 348 adlda_mode_enron_adlda_100_cores_8_seed_4717
# 221 221  adlda_mode_nips_adlda_100_cores_4_seed_4712
# 243 243  adlda_mode_nips_adlda_100_cores_2_seed_4716
# 286 286  adlda_mode_enron_adlda_20_cores_8_seed_4711
# 295 295  adlda_mode_enron_adlda_20_cores_8_seed_4713
# 298 298  adlda_mode_enron_adlda_20_cores_8_seed_4715
# 301 301  adlda_mode_enron_adlda_20_cores_8_seed_4716
# 307 307  adlda_mode_enron_adlda_20_cores_8_seed_4719
# 321 321  adlda_mode_enron_adlda_20_cores_8_seed_4721

# rem <- c(189,190,221,222,223,230,233,243,286,295,298,301,307,321,348)


# find_error(x = res_lik, FUN = function(X) any(X$typeTokenDensity[100:200] > 0.2))
# 272 272  adlda_mode_enron_adlda_20_cores_1_seed_4716
# 278 278  adlda_mode_enron_adlda_20_cores_2_seed_4716
# 281 281  adlda_mode_enron_adlda_20_cores_4_seed_4716
# 316 316 adlda_mode_enron_adlda_100_cores_1_seed_4716
# 320 320 adlda_mode_enron_adlda_100_cores_4_seed_4716
# 332 332 adlda_mode_enron_adlda_100_cores_2_seed_4716
# 347 347 adlda_mode_enron_adlda_100_cores_8_seed_4716

# find_error(x = res_lik, data_set="nips", k="20", algo="adlda", FUN=function(X) any(X$documentDensity[2:500] < 0.3))
#     no                                        txt
#175 175 adlda_mode_nips_adlda_20_cores_8_seed_4711
#176 176 adlda_mode_nips_adlda_20_cores_4_seed_4711


# New checks
# find_error(x = res_lik, FUN = function(X) any(X$loglik[2:length(X$loglik)] < -75000000))
#      no                                          txt
# 333 333 adlda_mode_enron_adlda_100_cores_8_seed_4721
# 334 334   adlda_mode_nips_adlda_20_cores_8_seed_4711

# find_error(x = res_lik, FUN = function(X) any(length(X$loglik) < 400))
# 329 329 adlda_mode_enron_adlda_100_cores_8_seed_4717
# 345 345  adlda_mode_enron_adlda_20_cores_8_seed_4715
# 346 346  adlda_mode_enron_adlda_20_cores_8_seed_4716
# 348 348  adlda_mode_enron_adlda_20_cores_8_seed_4719

# find_error(x = res_lik, data_set="nips", k="20", algo="adlda", FUN=function(X) any(X$documentDensity[2:500] < 0.3))
#      no                                        txt
# 334 334 adlda_mode_nips_adlda_20_cores_8_seed_4711

# find_error(x = res_lik, data_set="nips", k="20", algo="adlda", FUN=function(X) any(X$typeTokenDensity[100:500] > 0.22))
#      no                                        txt
# 342 342 adlda_mode_nips_adlda_20_cores_8_seed_4720

# find_error(x = res_lik, data_set="enron", k="20", algo="adlda", cores="8", FUN=function(X) any(X$typeTokenDensity[10:length(X$typeTokenDensity)] > 0.20))

#     no                                         txt
#340 340 adlda_mode_enron_adlda_20_cores_8_seed_4713
#346 346 adlda_mode_enron_adlda_20_cores_8_seed_4716

# find_error(x = res_lik, data_set="enron", k="20", algo="adlda", FUN=function(X) any(X$documentDensity[10:length(X$documentDensity)] > 0.30))

#      no                                         txt
#340 340 adlda_mode_enron_adlda_20_cores_8_seed_4713

# All together
# 333 333 adlda_mode_enron_adlda_100_cores_8_seed_4721
# 329 329 adlda_mode_enron_adlda_100_cores_8_seed_4717
# 345 345  adlda_mode_enron_adlda_20_cores_8_seed_4715
# 348 348  adlda_mode_enron_adlda_20_cores_8_seed_4719
# 334 334 adlda_mode_nips_adlda_20_cores_8_seed_4711
# 342 342 adlda_mode_nips_adlda_20_cores_8_seed_4720
# 340 340 adlda_mode_enron_adlda_20_cores_8_seed_4713
# 346 346 adlda_mode_enron_adlda_20_cores_8_seed_4716
#rem <- c(333,329,345,348,334,342,340,346)


# AD-LDA2 runs
#find_error(x = res_lik, data_set="enron", k="20", algo="adlda", FUN=function(X) any(X$loglik[10:length(X$loglik)] < -200000000))

#find_error(x = res_lik, data_set="nips", k="20", algo="adlda", FUN=function(X) any(X$loglik[10:length(X$loglik)] < -75000000))
#346 346 adlda_mode_enron_adlda_20_cores_8_seed_4715
#349 349 adlda_mode_enron_adlda_20_cores_8_seed_4713
#332 332 adlda_mode_nips_adlda_20_cores_8_seed_4711
#345 345 adlda_mode_nips_adlda_20_cores_8_seed_4720

#find_error(x = res_lik, cores = "8", data_set="enron", k="100", algo="adlda", FUN=function(X) any(X$documentDensity[length(X$documentDensity)] > 0.114))

#find_error(x = res_lik, cores = "8", data_set="enron", k="20", algo="adlda", FUN=function(X) any(X$documentDensity[length(X$documentDensity)] > 0.25))


data_set<- c("nips")
k <- c(100) # i<-1

algos <- c("adlda", "spalias")
algos_name <- c("AD-LDA", "PC-LDA")

y <- c("loglik", "typeTokenDensity", "documentDensity") # typeTokenDensity, documentDensity, loglik
y_ylab <- c("Log-likelihood", "Wordtype-topic sparsity", "Document-topic sparsity")
y_it <- list(c(100,4000), c(40,4900), c(40,4900))
#y_lim <- c(-15800000, -15200000) # nips 20
y_lim <- c(-15750000,-15000000) # nips 100
#y_lim <- c(-54200000, -53100000) # enron 20
#y_lim <- c(-54500000, -53500000) # enron 100



full_c_plts <- list()
for(i in seq_along(algos)){ # algo <- "adlda" i <-2
  full_c_plts[[i]] <- full_core_plt(x = res_lik, data_set = data_set, k = k, algo=algos[i], y = "loglik")
  full_c_plts[[i]] <- full_c_plts[[i]] + my_theme() + #ggtitle(algos_name[i]) +
    xlab("Iterations") + ylab("Log Marginal Posterior") + ylim(y_lim)
  ggsave(filename = paste0(tmp_folder,"fig_admode_lmp_",algos[i], "_", data_set,"_", k,".png"),
         full_c_plts[[i]], width = 6, height = 4)
}

full_c_plts[[1]] <- full_c_plts[[1]] + guides(colour=FALSE, linetype=FALSE)
full_c_plts[[2]] <- 
  full_c_plts[[2]] + ylab(NULL) + labs(colour = "Cores", linetype="Cores") +
  theme(legend.justification=c(1,0), legend.position=c(1,0))

y <- c("typeTokenDensity", "documentDensity") # typeTokenDensity, documentDensity, loglik
y_ylab <- c("Wordtype-topic sparsity", "Document-topic sparsity")
comp_plts <- list()
for(i in seq_along(y)){ # algo <- "adlda" i <-1
  comp_plts[[i]] <- comp_core_plt(x = res_lik, data_set = data_set, k = k, y = y[i])
  comp_plts[[i]] <- comp_plts[[i]] + my_theme() + ggtitle(y_ylab[i]) +
    xlab("Cores") + ylab("Non-zero elements (proportion)") + labs(colour = "Sampler", linetype="Sampler")
  comp_plts[[i]]$data$algo[comp_plts[[i]]$data$algo=="adlda"] <- "AD-LDA"
  comp_plts[[i]]$data$algo[comp_plts[[i]]$data$algo=="spalias"] <- "PC-LDA"
  ggsave(filename = paste0(tmp_folder,"fig_admode_sparsity_",y[i], "_", data_set,"_", k,".png"),
         comp_plts[[i]], width = 6, height = 4)
}

comp_plts[[1]] <- comp_plts[[1]] + guides(colour=FALSE, linetype=FALSE)
comp_plts[[2]] <- 
  comp_plts[[2]] + ylab(NULL) + labs(colour = "Sampler", linetype="Sampler")  +
  theme(legend.justification=c(1,-1), legend.position=c(1,0))
comp_plts[[2]]$data$algo[comp_plts[[2]]$data$algo=="adlda"] <- "AD-LDA"
comp_plts[[2]]$data$algo[comp_plts[[2]]$data$algo=="spalias"] <- "PC-LDA"

plt <- arrangeGrob(full_c_plts[[1]], full_c_plts[[2]], comp_plts[[1]], comp_plts[[2]], ncol=2)
ggsave(filename = paste0(tmp_folder,"fig_admode_", data_set,"_", k,".png"),
       plt, width = 6, height = 4)

