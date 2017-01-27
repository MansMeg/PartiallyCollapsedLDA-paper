#' Function to create bash files, config files and NSC Runs for the PUBMED experiment
#' 
#' @description
#'  Function to generate bash scripts and config files to run at NSC.
#'  The jar file assumes to be localed at the part_to_project.
#'  
#'  @inheritParams create_nsc_runs_ineff
#'  @param cores Vector with the number of cores to use
#'  @param samplers Vector with the different samplers to use
#'  @param huge_node Should the huge node be used?
#'  
#' @examples
#' create_nsc_runs_pubmed(java_seeds = 20150326, 
#'                          cores = c(64, 32), 
#'                          K = c(200, 400, 800), 
#'                          data_paths = "datasets/pubmed.txt",
#'                          samplers = c("spalias", "adlda"),
#'                          mail_to = "leif.jonsson[at]ericsson.com")
#'
#' @export
create_nsc_runs_pubmed <- function(path_to_project = "", folder="pubmed", java_seeds, K, data_paths, max_doc_buf_size=NULL, rare_word_limits=NULL, tfidf_vocab_size=NULL, cores=NULL, samplers, mail_to, huge_node=TRUE, mem, time_limit_h = NULL, alpha_beta_prior=NULL, account, call="PCPLDA.jar cc.mallet.topics.tui.ParallelLDA", iterations=1000){
  #  data_paths <- "datasets/pubmed.txt"
  #  K = c(200, 400, 800)
  #  java_seeds <- 20150326
  #  cores <- c(64, 32)
  # mail_to <- "leif.jonsson@ericsson.com"
  # samplers <- c("collapsed", "uncollapsed", "adlda")
  bash_script_file <- "#!/bin/bash
#
#SBATCH -J [actual_config]
#SBATCH -t [time_to_use]
#SBATCH --mem=[mb_to_use]
#SBATCH --mail-type=ALL
#SBATCH --mail-user=[mail_to]
#SBATCH --output=\"slurm-pubmed-%j-[actual_config].out\"
[extra]
[sbatch_cores]
#
# module load java/jre1.8.0_45
cat PCPLDA_jar_README.txt 
git log -n 1
java -version
sleep [sleep_sec]
"
  if(!is.null(cores)) {
    bash_script_file <- paste0(bash_script_file, "taskset -c [core_interval] ")
    if(!huge_node) sbatch_cores <- "#SBATCH -n [cores_to_use]" else sbatch_cores <- ""
  } else {
    cores <- 1
    sbatch_cores <- "#SBATCH -N 1"
    }
  bash_script_file <- paste0(bash_script_file, "java -Xmx[gb_to_use]g -cp [call] --run_cfg=[actual_config_path]")

  config_file <- "configs = [actual_config]
no_runs = 1
experiment_out_dir = [folder]

[[actual_config]]
title = [actual_config]
description = Pubmed experiment ([actual_config])
dataset = [data]
scheme = [sampler]
seed = [seed]
topics = [K]
alpha = [alpha]
beta = [beta]
iterations = [iter]
batches = [cores_to_use]
topic_batches = [cores_to_use]
[vocab_truncation]
topic_interval = 10
diagnostic_interval = -1
dn_diagnostic_interval = -1
results_size = 5
debug = 0
log_type_topic_density = true
log_document_density = true
[max_doc_buf_size]
"
  # Assertions
  if(is.null(rare_word_limits) & is.null(tfidf_vocab_size)) stop("Need to specify rare word limit or vocab size")
  if(!is.null(rare_word_limits)) vocab_truncation <- paste("rare_threshold =", rare_word_limits)
  if(!is.null(tfidf_vocab_size)) vocab_truncation <- paste("tfidf_vocab_size =", tfidf_vocab_size)
  stopifnot(length(data_paths) == length(vocab_truncation))
  stopifnot(is.null(max_doc_buf_size) | length(data_paths) == length(max_doc_buf_size))
  if(!is.null(max_doc_buf_size)) max_doc_buf_size <- paste("max_doc_buf_size =", max_doc_buf_size) else max_doc_buf_size <- rep("", length(data_paths))
  
  # Create folder for job files
  dir.create(paste0(path_to_project, "NSC"))
  dir.create(paste0(path_to_project, "NSC/job_files"))
  dir.create(paste0(path_to_project, "NSC/job_files/", folder))
  job_file_path <- paste0(path_to_project, "NSC/job_files/", folder)
  
  # Loop over K, data_sets, sampler and cores
  bash_path <- list()
  total_time <- 0
  sleep_sec <- 1
  par_mem <- mem
  dataset_no <- 0
  
  for (data_txt in data_paths){
    dataset_no <- dataset_no + 1
    for (k in K){ 
      for (sampler in samplers){
        for (cores_no in cores){    
          for (s in java_seeds){
            bash <- bash_script_file
            cfg <- config_file
            
            ## Different priors
            if (is.null(alpha_beta_prior)) {
              alpha <- beta <- 10/k
              } else {
                alpha <- alpha_beta_prior[1]
                beta <- alpha_beta_prior[2]
              }
            ##
            
            ## This part is a fix to handle runs @ Gamma
            if (k >= 10000 & !huge_node) {
              extra <- "#SBATCH -p fat\n"
              mem <- 128000
            } else {
              extra <- ""
              mem <- par_mem
            }

            ## This part is a fix to handle runs @ Gamma
            
            if (huge_node) extra <- "#SBATCH --reservation=huge -N1 --exclusive\n" else extra <- ""
            # Add account information
            extra <- paste0(extra, "#SBATCH -A ", account)
            
            cores_to_use <- cores_no
            
            if(sampler == "collapsed" & cores_no > 1) next()
            if(!file.exists(paste0(path_to_project, data_txt))) warning(paste0(paste0(path_to_project, data_txt), " do not exist."))
            data_name <- unlist(stringr::str_split(data_txt, "/|.txt"))
            data_name <- data_name[length(data_name) - 1]
            
            # K, data, sampler, cores
            # This needs to be first
            bash <- stringr::str_replace_all(string = bash, 
                                             pattern = "\\[sbatch_cores\\]", 
                                             replacement = sbatch_cores)
            
            cfg <- stringr::str_replace_all(string = cfg, 
                                            pattern = "\\[K\\]", 
                                            replacement = k)
            cfg <- stringr::str_replace_all(string = cfg, 
                                            pattern = "\\[iter\\]", 
                                            replacement = iterations)
            cfg <- stringr::str_replace_all(string = cfg, 
                                            pattern = "\\[folder\\]", 
                                            replacement = folder)
            cfg <- stringr::str_replace_all(string = cfg, 
                                            pattern = "\\[seed\\]", 
                                            replacement = as.character(s))
            cfg <- stringr::str_replace_all(string = cfg, 
                                            pattern = "\\[data\\]", 
                                            replacement = data_txt)
            cfg <- stringr::str_replace_all(string = cfg, 
                                            pattern = "\\[vocab_truncation\\]", 
                                            replacement = vocab_truncation[dataset_no])
            cfg <- stringr::str_replace_all(string = cfg, 
                                            pattern = "\\[max_doc_buf_size\\]", 
                                            replacement = max_doc_buf_size[dataset_no])
            bash <- stringr::str_replace_all(string = bash, 
                                             pattern = "\\[cores_to_use\\]", 
                                             replacement = as.character(cores_to_use))
            bash <- stringr::str_replace_all(string = bash, 
                                             pattern = "\\[call\\]", 
                                             replacement = call)
            bash <- stringr::str_replace_all(string = bash, 
                                             pattern = "\\[core_interval\\]", 
                                             replacement = core_interval_function(cores_to_use))
            cfg <- stringr::str_replace_all(string = cfg, 
                                            pattern = "\\[cores_to_use\\]", 
                                            replacement = as.character(cores_to_use))
            cfg <- stringr::str_replace_all(string = cfg, 
                                            pattern = "\\[sampler\\]", 
                                            replacement = sampler)
            cfg <- stringr::str_replace_all(string = cfg, 
                                            pattern = "\\[alpha\\]", 
                                            replacement = alpha)
            cfg <- stringr::str_replace_all(string = cfg, 
                                            pattern = "\\[beta\\]", 
                                            replacement = beta)
            bash <- stringr::str_replace_all(string = bash, 
                                             pattern = "\\[extra\\]", 
                                             replacement = extra)
            
            # Sleep
            bash <- stringr::str_replace_all(string = bash, 
                                             pattern = "\\[sleep_sec\\]", 
                                             replacement = as.character(sleep_sec))
            sleep_sec <- sleep_sec + 2
            
            # Actual config
            actual_config <- paste("large", data_name, k, sampler, "cores", cores_to_use, "seed", s, sep="_")
            bash <- stringr::str_replace_all(string = bash, 
                                             pattern = "\\[actual_config\\]", 
                                             replacement = actual_config)
            cfg <- stringr::str_replace_all(string = cfg, 
                                            pattern = "\\[actual_config\\]", 
                                            replacement = actual_config)
            # Actual config path
            actual_config_path <- paste0(job_file_path, "/", actual_config, ".cfg")
            bash <- stringr::str_replace_all(string = bash, 
                                             pattern = "\\[actual_config_path\\]", 
                                             replacement = actual_config_path)
            # Memory and time to use
            mb_to_use <- mem
            gb_to_use_java <- round(mb_to_use/1000) - round(mb_to_use/10000)
            bash <- stringr::str_replace_all(string = bash, pattern = "\\[mb_to_use\\]", replacement = mb_to_use)
            bash <- stringr::str_replace_all(string = bash, pattern = "\\[gb_to_use\\]", replacement = gb_to_use_java)
            if(is.null(time_limit_h)){
              time_to_use <- calc_time_usage_pubmed(k, data_name, cores_to_use)
            } else {
              time_to_use <- num_time_to_nsc_format(time_limit_h)
            }
            total_time <- total_time + as.numeric(substr(time_to_use,1,2))
            bash <- stringr::str_replace_all(string = bash, pattern = "\\[time_to_use\\]", replacement = time_to_use)
            bash <- stringr::str_replace_all(string = bash, pattern = "\\[mail_to\\]", replacement = mail_to)
            bash_path[[length(bash_path) + 1]] <- 
              paste0(job_file_path, "/", actual_config, ".sh")
            if(bash=="NA" | is.na(bash)) stop(paste(data_txt, k, sampler, cores_no, s))
            writeLines(text = bash, bash_path[[length(bash_path)]])
            if(cfg=="NA" | is.na(cfg)) stop(paste(data_txt, k, sampler, cores_no, s))
            writeLines(text = cfg, actual_config_path)
          }
        }
      }
    }
  }
  # Write batch files
  all_jobs <- paste0("# Run all jobs\n",paste(paste("sbatch", unlist(bash_path), "\n"), collapse = ""))
  writeLines(text = all_jobs, paste0(job_file_path, "/run_jobs.sh"))
  return(total_time)
}

calc_time_usage_pubmed <- function(k, data_name, cores_to_use){
  res <- 72
  if(grepl("p10", data_name)){ # 10 % pubmed
    if(k == 10) res <- 4
    if(k == 100) res <- 8
    if(k == 1000) res <- 31
    if(k == 10000) res <- 71
  } else { # Full pubmed
    if(k == 10) res <- 21
    if(k == 100) res <- 21
    if(k == 1000) res <- 71
    if(k == 10000) res <- 71
  }
  paste0(ifelse(res < 10, paste0("0",as.character(res)), as.character(res)), ":00:00")
}
