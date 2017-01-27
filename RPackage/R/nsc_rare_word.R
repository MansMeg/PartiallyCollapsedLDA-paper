#' Function to create bash files, config files and NSC Runs for Spalias experiment
#' 
#' @description
#'  Function to generate bash scripts and config files to run at NSC.
#'  The jar file assumes to be localed at the part_to_project.
#'  
#'  @inheritParams create_nsc_runs_ineff
#'  @param cores Number of cores to use
#'
#'  @examples
#'  create_nsc_runs_rarewords(java_seeds=20150326 + 0:2, 
#'                            K=c(20,100), 
#'                            data_paths = c("datasets/enron_random.txt", "datasets/nips_random.txt"), 
#'                            rare_words = list(c(14,43,104,193), c(12,30,62,108)), 
#'                            mail_to="mons.magnusson[at]gmail.com")
#'              
#'  @export
create_nsc_runs_rarewords <- function(path_to_project = "", 
                                      java_seeds, 
                                      K,
                                      rare_words,
                                      data_paths, 
                                      mail_to,
                                      cores = 8){
  samplers <- c("adlda", "spalias")
  # data_paths <- c("datasets/enron_random.txt", "datasets/nips_random.txt")
  bash_script_file <- "#!/bin/bash
#
#SBATCH -J [actual_config]
#SBATCH -t [time_to_use]
#SBATCH -A liu-2014-00036-36
#SBATCH --mem=[memory_to_use]
#SBATCH -n [cores_to_use]
#SBATCH --mail-type=ALL
#SBATCH --mail-user=[mail_to]
#SBATCH --output=\"slurm-rarewords-%j.out\"
#
module load java/jre1.7.0_04
sleep [sleep_sec]
java -cp PCPLDA-1.0.9.jar tgs.runnables.SpaliasExperiment --run_cfg=[actual_config_path]
"
  
  config_file <- "configs = [actual_config]
no_runs = 1

[[actual_config]]
title = [actual_config]
description = Rare word speedup ([actual_config])
dataset = [data]
scheme = [sampler]
seed = [seed]
topics = [K]
alpha = 0.1
beta = 0.01
iterations = 5000
batches = [cores_to_use]
topic_batches = [cores_to_use_topic]
rare_threshold = [rare_word]
topic_interval = 10
diagnostic_interval = -1
dn_diagnostic_interval = -1
results_size = 5
debug = 0
"
  
  # Create folder for job files
  dir.create(paste0(path_to_project, "NSC"))
  dir.create(paste0(path_to_project, "NSC/job_files"))
  dir.create(paste0(path_to_project, "NSC/job_files/rareword"))
  job_file_path <- paste0(path_to_project, "NSC/job_files/rareword")
  
  # Loop over K, data_sets, sampler and cores
  bash_path <- list()
  total_time <- 0
  sleep_sec <- 1
  cores_no <- cores
  
  for (d in seq_along(data_paths)){
    for (k in K){ 
      for (sampler in samplers){
        for (r in seq_along(rare_words[[d]])){
          for (s in java_seeds){
            data_txt <- data_paths[d]
            
            bash <- bash_script_file
            cfg <- config_file
            cores_to_use <- cores_no
            cores_to_use_topic <- max(1, cores_no - 1)
            if(!file.exists(paste0(path_to_project, data_txt))) warning(paste0(paste0(path_to_project, data_txt), " do not exist."))
            data_name <- unlist(stringr::str_split(data_txt, "/|.txt"))
            data_name <- data_name[length(data_name) - 1]
            
            # K, data, sampler, cores,
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
                                            pattern = "\\[sampler\\]", 
                                            replacement = as.character(sampler))
            cfg <- stringr::str_replace_all(string = cfg, 
                                            pattern = "\\[cores_to_use_topic\\]", 
                                            replacement = as.character(cores_to_use_topic))
            cfg <- stringr::str_replace_all(string = cfg, 
                                            pattern = "\\[rare_word\\]", 
                                            replacement = as.character(rare_words[[d]][r]))

            # Sleep
            bash <- stringr::str_replace_all(string = bash, 
                                             pattern = "\\[sleep_sec\\]", 
                                             replacement = as.character(sleep_sec))
            sleep_sec <- sleep_sec + 2
            
            # Actual config
            actual_config <- paste("rarewords", data_name, k, substr(sampler,1,5), "rw", rare_words[[d]][r], "seed", s, sep="_")
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
            memory_to_use <- calc_memory_usage(k, data_name)
            time_to_use <- calc_time_usage_spalias(k, data_name, cores_to_use)
            total_time <- total_time + as.numeric(substr(time_to_use,1,2))
            bash <- stringr::str_replace_all(string = bash, pattern = "\\[memory_to_use\\]", replacement = memory_to_use)
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
  }
  # Write batch files
  all_jobs <- paste0("# Run all jobs\n", paste(paste("sbatch", unlist(bash_path), "\n"), collapse = ""))
  writeLines(text = all_jobs, paste0(job_file_path, "/run_jobs.sh"))
  return(total_time)
}

