
#' Function to create bash files, config files and NSC Runs
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
#'  @examples
#'  create_nsc_runs_speedup(java_seeds = 20150326, 
#'                          cores = c(64, 32, 16, 8, 4, 1), 
#'                          K = c(20, 100), 
#'                          data_paths = c("datasets/enron.txt", "datasets/nips.txt"),
#'                          samplers = c("collapsed", "uncollapsed", "adlda"),
#'                          mail_to = "leif.jonsson[at]ericsson.com")
#'                          
#'  @export
create_nsc_runs_speedup <- function(path_to_project = "", 
                                    java_seeds, 
                                    K, 
                                    data_paths, 
                                    cores,
                                    iterations=20000,
                                    directory="speedup",
                                    samplers, 
                                    mem,
                                    mail_to, 
                                    huge_node=TRUE){
  bash_script_file <- "#!/bin/bash
#
#SBATCH -J [actual_config]
#SBATCH -t [time_to_use]
#SBATCH --mem=[mb_to_use]
#SBATCH -n [cores_to_use]
#SBATCH --mail-type=ALL
#SBATCH --mail-user=[mail_to]
#SBATCH --output=\"slurm-speedup-%j-[actual_config].out\"
[extra]
#
module load java/jre1.8.0_45
sleep [sleep_sec]
taskset -c [core_interval] java -Xmx[gb_to_use]g -cp PCPLDA-1.1.8.jar tgs.runnables.ParallelLDA --run_cfg=[actual_config_path]
"
  
  config_file <- "configs = [actual_config]
no_runs = 1
experiment_out_dir = [directory]
  
[[actual_config]]
title = [actual_config]
description = Speedup experiment ([actual_config])
dataset = [data]
scheme = [sampler]
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
batch_building_scheme = utils.randomscan.document.EvenSplitBatchBuilder
debug = 0
log_type_topic_density = true
log_document_density = true
  
"
  
  # Create folder for job files
  dir.create(paste0(path_to_project, "NSC"))
  dir.create(paste0(path_to_project, "NSC/job_files"))
  dir.create(paste0(path_to_project, "NSC/job_files/", directory))
  job_file_path <- paste0(path_to_project, "NSC/job_files/", directory)
  
  # Loop over K, data_sets, sampler and cores
  bash_path <- list()
  total_time <- 0
  sleep_sec <- 1
  
  for (data_txt in data_paths){
    for (k in K){ 
      for (sampler in samplers){
        for (cores_no in cores){    
          for (s in java_seeds){
            bash <- bash_script_file
            cfg <- config_file
            if (huge_node) extra <- "#SBATCH -p huge\n#SBATCH -A liu-2014-00036-36-huge" else extra <- "#"
            cores_to_use <- cores_no
            cores_to_use_topic <- max(1, cores_no - 1)
            if(sampler == "collapsed" & cores_no > 1) next()
            if(!file.exists(paste0(path_to_project, data_txt))) warning(paste0(paste0(path_to_project, data_txt), " do not exist."))
            data_name <- unlist(stringr::str_split(data_txt, "/|.txt"))
            data_name <- data_name[length(data_name) - 1]
            
            # K, data, sampler, cores
            cfg <- stringr::str_replace_all(string = cfg, 
                                            pattern = "\\[K\\]", 
                                            replacement = k)
            cfg <- stringr::str_replace_all(string = cfg, 
                                            pattern = "\\[directory\\]", 
                                            replacement = directory)
            cfg <- stringr::str_replace_all(string = cfg, 
                                            pattern = "\\[iterations\\]", 
                                            replacement = as.character(iterations))
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
            bash <- stringr::str_replace_all(string = bash, 
                                             pattern = "\\[extra\\]", 
                                             replacement = extra)
            
            # Sleep
            bash <- stringr::str_replace_all(string = bash, 
                                             pattern = "\\[sleep_sec\\]", 
                                             replacement = as.character(sleep_sec))
            sleep_sec <- sleep_sec + 2
            
            # Actual config
            actual_config <- paste("speed", data_name, k, sampler, "cores", cores_to_use, "seed", s, sep="_")
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
            
            time_to_use <- calc_time_usage_speedup(k, data_name, cores_to_use)
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
  return(total_time)
}

calc_time_usage_speedup <- function(k, data_name, cores_to_use){
  if (stringr::str_detect(string = data_name, "kos")) {res <- 8}
  if (stringr::str_detect(string = data_name, "nips")) {res <- 8}
  if (stringr::str_detect(string = data_name, "enron")) {res <- 24}
#  res <- max(1, round(res * (k/20))) + 2
  paste0(ifelse(res < 10, paste0("0",as.character(res)), as.character(res)), ":00:00")
}

core_interval_function <- function(n){
  if(n == 1) {
    return("0")
  } else {
    return(paste("0-", n-1, sep=""))
  }
}

