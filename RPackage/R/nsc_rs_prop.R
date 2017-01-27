#' Function to create bash files, config files and NSC Runs for RS-prop experiment
#' 
#' @description
#'  Function to generate runs for random scan tokens proportional to size.
#'  
#'  @inheritParams create_nsc_runs_ineff
#'  @param cores Vector with the number of cores to use
#'  @param samplers Vector with the different samplers to use
#'  @param samplers Vector with the different samplers to use
#'  
#'  @examples
#'  create_nsc_runs_rs_prop(java_seeds = 20150326 + (0:4), 
#'                     K = c(20, 100), 
#'                     data_paths = c("datasets/enron.txt", "datasets/nips.txt"),
#'                     step_sizes = c(1, 100, 200, 500, 1000, 2000),
#'                     mail_to = "mons.magnusson[at]gmail.com, leif.jonsson[at]ericsson.com")
#'                          
#'  @export
create_nsc_runs_rs_prop <- function(path_to_project = "", 
                               java_seeds, 
                               K, 
                               data_paths, 
                               mail_to,
                               mem,
                               cores = 8, 
                               step_sizes){

  bash_script_file <- "#!/bin/bash
#
#SBATCH -J [actual_config]
#SBATCH -t [time_to_use]
#SBATCH -A liu-2014-00036-36
#SBATCH --mem=[mb_to_use]
#SBATCH -n [cores_to_use]
#SBATCH --mail-type=ALL
#SBATCH --mail-user=[mail_to]
#SBATCH --output=\"slurm-rs_prop-%j-[actual_config].out\"
#
module load java/jre1.8.0_45
sleep [sleep_sec]
java -Xmx[gb_to_use]g -cp PCPLDA-1.1.4.jar tgs.runnables.ParallelLDA --run_cfg=[actual_config_path]
"
  
  config_file <- "configs = [actual_config]
no_runs = 1
experiment_out_dir = rs_prop

[[actual_config]]
title = [actual_config]
description = RS prop. experiment ([actual_config])
dataset = [data]
scheme = spalias
seed = [seed]
topics = [K]
alpha = 0.1
beta = 0.01
iterations = 10000
batches = [cores_to_use]
topic_batches = [cores_to_use_topic]
rare_threshold = 10
topic_interval = 10
diagnostic_interval = -1
dn_diagnostic_interval = -1
results_size = 5
topic_index_building_scheme = utils.randomscan.topic.ProportionalTopicIndexBuilder
proportional_ib_skip_step = [step_size]
debug = 0
log_type_topic_density = true
log_document_density = true
"
  
  # Create folder for job files
  dir.create(paste0(path_to_project, "NSC"))
  dir.create(paste0(path_to_project, "NSC/job_files"))
  dir.create(paste0(path_to_project, "NSC/job_files/rs_prop"))
  job_file_path <- paste0(path_to_project, "NSC/job_files/rs_prop")
  
  # Loop over K, data_sets, sampler and cores
  bash_path <- list()
  total_time <- 0
  sleep_sec <- 1
  cores_no <- cores
  
  for (data_txt in data_paths){
    for (k in K){ 
      for (step_size in step_sizes){
        for (s in java_seeds){
          bash <- bash_script_file
          cfg <- config_file
          cores_to_use <- cores_no
          cores_to_use_topic <- max(1, cores_no - 1)
          if(!file.exists(paste0(path_to_project, data_txt))) warning(paste0(paste0(path_to_project, data_txt), " do not exist."))
          data_name <- unlist(stringr::str_split(data_txt, "/|.txt"))
          data_name <- data_name[length(data_name) - 1]
          
          # K, data, cores, step_size
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
          cfg <- stringr::str_replace_all(string = cfg, 
                                          pattern = "\\[cores_to_use\\]", 
                                          replacement = as.character(cores_to_use))
          cfg <- stringr::str_replace_all(string = cfg, 
                                          pattern = "\\[cores_to_use_topic\\]", 
                                          replacement = as.character(cores_to_use_topic))
          cfg <- stringr::str_replace_all(string = cfg, 
                                          pattern = "\\[step_size\\]", 
                                          replacement = step_size)
          
          # Sleep
          bash <- stringr::str_replace_all(string = bash, 
                                           pattern = "\\[sleep_sec\\]", 
                                           replacement = as.character(sleep_sec))
          sleep_sec <- sleep_sec + 2
          
          # Actual config
          actual_config <- paste("rs_prop", data_name, k, "step", step_size, "seed", s, sep="_")
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
          
          time_to_use <- calc_time_usage_rs(k, data_name, cores_to_use)
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


calc_time_usage_rs <- function(k, data_name, cores_to_use){
  if (stringr::str_detect(string = data_name, "kos")) {res <- 15}
  if (stringr::str_detect(string = data_name, "nips")) {
    if(k == 20){res <- 2} else if (k == 100) {res <- 4}
  }
  if (stringr::str_detect(string = data_name, "enron")) {
    if(k == 20){res <- 8} else if (k == 100) {res <- 10} else if (k > 100) {res <- 20} 
  }
  paste0(ifelse(res < 10, paste0("0",as.character(res)), as.character(res)), ":00:00")
}
