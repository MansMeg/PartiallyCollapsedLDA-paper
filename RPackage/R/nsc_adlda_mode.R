#' Function to create bash files, config files and NSC Runs for AD-LDA mode test
#' 
#' @description
#'  Function to generate bash scripts and config files to run at NSC.
#'  The jar file assumes to be localed at the part_to_project.
#'  The purpose of the experiment is to see if there is differences in modes
#'  at different cores.
#'  
#'  @inheritParams create_nsc_runs_ineff
#'  @param cores Vector with the number of cores to use
#'  
#'  @examples
#'  create_nsc_runs_adlda_mode(java_seeds = 20150326, 
#'                          cores = c(8, 4, 2, 1), 
#'                          K = c(20, 100), 
#'                          data_paths = c("datasets/enron.txt", "datasets/nips.txt"),
#'                          mail_to = "leif.jonsson[at]ericsson.com")
#'                          
#'  @export
create_nsc_runs_adlda_mode <- function(path_to_project = "", java_seeds, mem, K, data_paths, cores, mail_to, samplers){
  # data_paths <- c("datasets/enron.txt", "datasets/nips.txt")
  # K <- c(20, 100)
  # java_seeds <- 20150326:20150329
  # cores <- c(8, 4, 2, 1)
  # mail_to <- "leif.jonsson@ericsson.com"
  bash_script_file <- "#!/bin/bash
#
#SBATCH -J [actual_config]
#SBATCH -t [time_to_use]
#SBATCH --mem=[mb_to_use]
#SBATCH -n [cores_to_use]
#SBATCH --mail-type=ALL
#SBATCH --mail-user=[mail_to]
#SBATCH -A liu-2014-00036-36
#SBATCH --output=\"slurm-adlda_mode-%j-[actual_config].out\"

module load java/jre1.8.0_45
sleep [sleep_sec]
taskset -c [core_interval] java -Xmx[gb_to_use]g -cp PCPLDA-1.1.4.jar tgs.runnables.ParallelLDA --run_cfg=[actual_config_path]
"

  config_file <- "configs = [actual_config]
no_runs = 1
experiment_out_dir = adlda_mode
  
[[actual_config]]
title = [actual_config]
description = ADLDA mode experiment ([actual_config])
dataset = [outdata]
scheme = [sampler]
seed = [seed]
topics = [K]
alpha = 0.1
beta = 0.01
iterations = 5000
batches = [cores_to_use]
topic_batches = [cores_to_use_topic]
rare_threshold = 10
topic_interval = 10
diagnostic_interval = -1
dn_diagnostic_interval = -1
results_size = 5
batch_building_scheme = utils.randomscan.document.EvenSplitBatchBuilder
debug = 0
log_type_topic_density = true
log_document_density = true
"

  # Create folder for job files
  dir.create(paste0(path_to_project, "NSC"))
  dir.create(paste0(path_to_project, "NSC/job_files"))
  dir.create(paste0(path_to_project, "NSC/job_files/adlda_mode"))
  job_file_path <- paste0(path_to_project, "NSC/job_files/adlda_mode")
  
  # Loop over K, data_sets and cores
  bash_path <- list()
  total_time <- 0
  sleep_sec <- 1
  for (sampler in samplers){
  for (data_txt in data_paths){
    for (k in K){ 
      for (cores_no in cores){    
        for (s in java_seeds){
          bash <- bash_script_file
          cfg <- config_file
          cores_to_use <- cores_no
          cores_to_use_topic <- max(1, cores_no - 1)
          if(!file.exists(paste0(path_to_project, data_txt))) warning(paste0(paste0(path_to_project, data_txt), " do not exist."))
          data_name_split <- unlist(stringr::str_split(data_txt, "/|.txt"))
          data_name <- data_name_split[length(data_name_split) - 1]
          outdata_txt <- stringr::str_replace_all(data_txt, pattern = data_name, replacement = paste(data_name, "random", s, sep = "_"))
          # K, data, cores
          cfg <- stringr::str_replace_all(string = cfg, 
                                          pattern = "\\[K\\]", 
                                          replacement = k)
          cfg <- stringr::str_replace_all(string = cfg, 
                                          pattern = "\\[seed\\]", 
                                          replacement = as.character(s))
          bash <- stringr::str_replace_all(string = bash, 
                                           pattern = "\\[seed\\]", 
                                           replacement = as.character(s))
          cfg <- stringr::str_replace_all(string = cfg, 
                                          pattern = "\\[outdata\\]", 
                                          replacement = outdata_txt)
          bash <- stringr::str_replace_all(string = bash, 
                                           pattern = "\\[data\\]", 
                                           replacement = data_txt)
          bash <- stringr::str_replace_all(string = bash, 
                                           pattern = "\\[outdata\\]", 
                                           replacement = outdata_txt)
          bash <- stringr::str_replace_all(string = bash, 
                                           pattern = "\\[cores_to_use\\]", 
                                           replacement = as.character(cores_to_use))
          bash <- stringr::str_replace_all(string = bash, 
                                           pattern = "\\[core_interval\\]", 
                                           replacement = core_interval_function(cores_to_use))
          cfg <- stringr::str_replace_all(string = cfg, 
                                          pattern = "\\[cores_to_use\\]", 
                                          replacement = as.character(cores_to_use))
          cfg <- stringr::str_replace_all(string = cfg, 
                                          pattern = "\\[cores_to_use_topic\\]", 
                                          replacement = as.character(cores_to_use_topic))
          cfg <- stringr::str_replace_all(string = cfg, 
                                          pattern = "\\[sampler\\]", 
                                          replacement = sampler)
          
          # Sleep
          bash <- stringr::str_replace_all(string = bash, 
                                           pattern = "\\[sleep_sec\\]", 
                                           replacement = as.character(sleep_sec))
          sleep_sec <- sleep_sec + 2
          
          # Actual config
          actual_config <- paste("adlda_mode", data_name, sampler, k, "cores", cores_to_use, "seed", s, sep="_")
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
          
          time_to_use <- calc_time_usage_adlda_mode(k, data_name, cores_to_use)
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
  }
  # Write batch files
  all_jobs <- paste0("# Run all jobs\n",paste(paste("sbatch", unlist(bash_path), "\n"), collapse = ""))
  writeLines(text = all_jobs, paste0(job_file_path, "/run_jobs.sh"))
  
  
  # Create data
  create_data_script_file <- "#!/bin/bash
#
#SBATCH -J Create data
#SBATCH -t 00:30:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=[mail_to]
#SBATCH -A liu-2014-00036-36
#SBATCH --output=\"slurm-create-data-%j.out\"
module load R/3.1.1
"
  create_data_script_file <- stringr::str_replace_all(string = create_data_script_file, pattern = "\\[mail_to\\]", replacement = mail_to)
  
  r_create_data <- list()
  r_rm_data <- list()  
  r_code_create <- "R CMD BATCH --vanilla '--args indata=\"[data]\" outdata=\"[outdata]\" seed=[seed]' R/shuffle_corpus.R Rout/[out].Rout"
  
  for (data_txt in data_paths){
    for (s in java_seeds){
      data_name_split <- unlist(stringr::str_split(data_txt, "/|.txt"))
      data_name <- data_name_split[length(data_name_split) - 1]
      outdata_txt <- stringr::str_replace_all(data_txt, pattern = data_name, replacement = paste(data_name, "random", s, sep = "_"))
      out <- paste(data_name, "random", s, sep = "_")
      tmp <- stringr::str_replace_all(string = r_code_create, 
                                      pattern = "\\[data\\]", 
                                      replacement = data_txt)
      tmp <- stringr::str_replace_all(string = tmp, 
                                      pattern = "\\[seed\\]", 
                                      replacement = as.character(s))
      tmp <- stringr::str_replace_all(string = tmp, 
                                      pattern = "\\[outdata\\]", 
                                      replacement = outdata_txt)
      tmp <- stringr::str_replace_all(string = tmp, 
                                      pattern = "\\[out\\]", 
                                      replacement = out)
      r_create_data[[length(r_create_data) + 1]] <- tmp
      r_rm_data[[length(r_rm_data) + 1]] <- paste0("rm ", outdata_txt)
    }
  }
  create_data_file <- paste0(create_data_script_file,
                             paste(paste(unlist(r_create_data), "\n"), collapse = ""))
  writeLines(text = create_data_file, paste0(job_file_path, "/data_creation.sh"))

  remove_data_file <- paste0("# Remove data\n", paste(paste(unlist(r_rm_data), "\n"), collapse = ""))
  writeLines(text = remove_data_file, paste0(job_file_path, "/data_remove.sh"))

  return(total_time)
}


calc_time_usage_adlda_mode <- function(k, data_name, cores_to_use){
  if (stringr::str_detect(string = data_name, "nips")) {res <- 8}
  if (stringr::str_detect(string = data_name, "enron")) {res <- 24}
  paste0(ifelse(res < 10, paste0("0",as.character(res)), as.character(res)), ":00:00")
}
