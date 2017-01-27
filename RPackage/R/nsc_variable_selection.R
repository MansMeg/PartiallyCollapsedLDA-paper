#' Function to create bash files, config files and NSC Runs for VS-prop experiment
#' 
#' @description
#'  Function to generate runs for random scan tokens proportional to size.
#'  
#' @inheritParams create_nsc_runs_ineff
#' @param cores Vector with the number of cores to use
#' @param samplers Vector with the different samplers to use
#' @param samplers Vector with the different samplers to use
#'                          
#' @export
create_nsc_runs_vs <- function(path_to_project = "",
                               folder,
                               java_seeds, 
                               K, 
                               data_paths, 
                               h,
                               mail_to,
                               mem,
                               cores = 16, 
                               pis, 
                               account,
                               iterations = 5000){

  stopifnot(length(h) == length(data_paths))
  
  bash_script_file <- "#!/bin/bash
#
#SBATCH -J [actual_config]
#SBATCH -t [time_to_use]
#SBATCH --mem=[mb_to_use]
#SBATCH -n [cores_to_use]
#SBATCH --mail-type=ALL
#SBATCH --mail-user=[mail_to]
#SBATCH --output=\"slurm-vs-%j-[actual_config].out\"
[extra]
#
cat PCPLDA_jar_README.txt 
git log -n 1
java -version
sleep [sleep_sec]
java -Xmx[gb_to_use]g -cp PCPLDA.jar cc.mallet.topics.tui.ParallelLDA --run_cfg=[actual_config_path]
"
  
  config_file <- "configs = [actual_config]
no_runs = 1
experiment_out_dir = [folder]

[[actual_config]]
title = [actual_config]
description = Variable selection experiment ([actual_config])
dataset = [data]
scheme = nzvsspalias
seed = [seed]
topics = [K]
alpha = 0.1
beta = 0.01
iterations = [iterations]
batches = [cores_to_use]
topic_batches = [cores_to_use_topic]
rare_threshold = 10
topic_interval = 10
diagnostic_interval = -1
dn_diagnostic_interval = -1
results_size = 5
log_phi_density = true
variable_selection_prior = [pi]
debug = 0
log_type_topic_density = true
log_document_density = true
"
  

  # Create folder for job files
  dir.create(paste0(path_to_project, "NSC"))
  dir.create(paste0(path_to_project, "NSC/job_files"))
  dir.create(paste0(path_to_project, "NSC/job_files/", folder))
  job_file_path <- paste0(path_to_project, "NSC/job_files/", folder)
  
  # Loop over K, data_sets, sampler and cores
  bash_path <- list()
  total_time <- 0
  sleep_sec <- 1
  cores_no <- cores
  
  for (i in seq_along(data_paths)){
    for (k in K){ 
      for (pi in pis){
        for (s in java_seeds){
          data_txt <- data_paths[i]
          
          bash <- bash_script_file
          cfg <- config_file
          cores_to_use <- cores_no
          cores_to_use_topic <- max(1, cores_no - 1)
          if(!file.exists(paste0(path_to_project, data_txt))) warning(paste0(paste0(path_to_project, data_txt), " do not exist."))
          data_name <- unlist(stringr::str_split(data_txt, "/|.txt"))
          data_name <- data_name[length(data_name) - 1]
          
          # Extra arguments
          extra <- ""
          extra <- paste0(extra, "#SBATCH -A ", account)
          
          # K, data, cores
          cfg <- stringr::str_replace_all(string = cfg, 
                                          pattern = "\\[K\\]", 
                                          replacement = k)
          cfg <- stringr::str_replace_all(string = cfg, 
                                          pattern = "\\[seed\\]", 
                                          replacement = as.character(s))
          cfg <- stringr::str_replace_all(string = cfg, 
                                          pattern = "\\[data\\]", 
                                          replacement = data_txt)
          bash <- stringr::str_replace_all(string = bash, 
                                           pattern = "\\[cores_to_use\\]", 
                                           replacement = as.character(cores_to_use))
          bash <- stringr::str_replace_all(string = bash, 
                                           pattern = "\\[extra\\]", 
                                           replacement = as.character(extra))
          cfg <- stringr::str_replace_all(string = cfg, 
                                          pattern = "\\[cores_to_use\\]", 
                                          replacement = as.character(cores_to_use))
          cfg <- stringr::str_replace_all(string = cfg, 
                                          pattern = "\\[cores_to_use_topic\\]", 
                                          replacement = as.character(cores_to_use_topic))
          cfg <- stringr::str_replace_all(string = cfg, 
                                          pattern = "\\[pi\\]", 
                                          replacement = pi)
          cfg <- stringr::str_replace_all(string = cfg, 
                                          pattern = "\\[folder\\]", 
                                          replacement = folder)
          
          # Sleep
          bash <- stringr::str_replace_all(string = bash, 
                                           pattern = "\\[sleep_sec\\]", 
                                           replacement = as.character(sleep_sec))
          sleep_sec <- sleep_sec + 2
          
          # Actual config
          
          actual_config <- paste("vs", data_name, k, "pi", str_replace_all(as.character(pi), "\\.", ""), "seed", s, sep="_")
          bash <- stringr::str_replace_all(string = bash, 
                                           pattern = "\\[actual_config\\]", 
                                           replacement = actual_config)
          cfg <- stringr::str_replace_all(string = cfg, 
                                          pattern = "\\[actual_config\\]", 
                                          replacement = actual_config)
          cfg <- stringr::str_replace_all(string = cfg, 
                                          pattern = "\\[iterations\\]", 
                                          replacement = iterations)
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
          
          time_to_use <- calc_time_usage_vs(h[i])
          total_time <- total_time + as.numeric(substr(time_to_use,1,2))
          bash <- stringr::str_replace_all(string = bash, pattern = "\\[time_to_use\\]", replacement = time_to_use)
          bash <- stringr::str_replace_all(string = bash, pattern = "\\[mail_to\\]", replacement = mail_to)
          bash_path[[length(bash_path) + 1]] <- 
            paste0(job_file_path, "/", actual_config, ".sh")
          writeLines(text = bash, bash_path[[length(bash_path)]])
          writeLines(text = cfg, actual_config_path)
        }
      }
    }
  }
  # Write batch files
  all_jobs <- paste0("# Run all jobs\n",paste(paste("sbatch", unlist(bash_path), "\n"), collapse = ""))
  writeLines(text = all_jobs, paste0(job_file_path, "/run_jobs.sh"))
  return(total_time)
}


calc_time_usage_vs <- function(h){
  paste0(ifelse(h < 10, paste0("0",as.character(h)), as.character(h)), ":00:00")
}
