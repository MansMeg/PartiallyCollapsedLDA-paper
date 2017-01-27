#' Function to create bash files, config files and NSC Runs for the PRIOR experiment
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
#'  create_nsc_runs_prior(java_seeds = 20150326, 
#'                          K = c(100, 200), 
#'                          data_paths = "datasets/enron.txt",
#'                          samplers = c("spalias", "adlda"),
#'                          alpha = c(0.1, 0.01),
#'                          beta = c(0.1, 0.01),
#'                          mail_to = "leif.jonsson[at]ericsson.com")
#'                          
#'  @export
create_nsc_runs_prior <- function(path_to_project = "", 
                                   java_seeds, 
                                   K, 
                                   data_paths, 
                                   alpha,
                                   beta,                                  
                                   cores_no = 8,
                                   h = 5, 
                                   mem,
                                   samplers, 
                                   mail_to){
  # path_ <- "datasets/pubmed.txt" 
  # data_paths <- "datasets/pubmed.txt"
  #  K = c(200, 400, 800)
  #  java_seeds <- 20150326
  #  cores <- c(64, 32)
  # mail_to <- "leif.jonsson@ericsson.com"
  # samplers <- c("collapsed", "uncollapsed", "adlda")
  bash_script_file <- "#!/bin/bash
#
#SBATCH -J [actual_config]
#SBATCH -t [time_to_use]
#SBATCH -A liu-2014-00036-36
#SBATCH --mail-type=ALL
#SBATCH --mail-user=[mail_to]
#SBATCH --mem=[mb_to_use]
#SBATCH -n [cores_to_use]
#SBATCH --output=\"slurm-priors-%j-[actual_config].out\"
#
module load java/jre1.8.0_45
sleep [sleep_sec]
java -Xmx[gb_to_use]g -cp PCPLDA-1.1.4.jar tgs.runnables.ParallelLDA --run_cfg=[actual_config_path]
"
  
  config_file <- "configs = [actual_config]
no_runs = 1
experiment_out_dir = prior

[[actual_config]]
title = [actual_config]
description = Prior experiment ([actual_config])
dataset = [data]
scheme = [sampler]
seed = [seed]
topics = [K]
alpha = [alpha]
beta = [beta]
iterations = 1000
batches = [cores_to_use]
topic_batches = [cores_to_use_topic]
rare_threshold = 10
topic_interval = 5
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
  dir.create(paste0(path_to_project, "NSC/job_files/prior"))
  job_file_path <- paste0(path_to_project, "NSC/job_files/prior")
  
  # Loop over K, data_sets, sampler, alpha, beta
  bash_path <- list()
  total_time <- 0
  sleep_sec <- 1
  
  
  
  for (data_txt in data_paths){
    if(!file.exists(paste0(path_to_project, data_txt))) warning(paste0(paste0(path_to_project, data_txt), " do not exist."))
    for (k in K){ 
      for (sampler in samplers){
        for (a in alpha){
        for (b in beta){    
          for (s in java_seeds){
            bash <- bash_script_file
            cfg <- config_file
            cores_to_use <- cores_no
            cores_to_use_topic <- max(1, cores_no - 1)
            data_name <- unlist(stringr::str_split(data_txt, "/|.txt"))
            data_name <- data_name[length(data_name) - 1]
            
            # K, data, sampler, cores, a, b
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
                                            pattern = "\\[alpha\\]", 
                                            replacement = as.character(a))
            cfg <- stringr::str_replace_all(string = cfg, 
                                            pattern = "\\[beta\\]", 
                                            replacement = as.character(b))
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
            actual_config <- paste("prior", data_name, k, sampler, "a", a, "b", b, "seed", s, sep="_")
            actual_config <- str_replace_all(string = actual_config, pattern = "\\.", "")
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
            total_time <- total_time + h

            time_to_use <- calc_time_h(h)
            bash <- stringr::str_replace_all(string = bash, pattern = "\\[mb_to_use\\]", replacement = mb_to_use)
            bash <- stringr::str_replace_all(string = bash, pattern = "\\[gb_to_use\\]", replacement = gb_to_use_java)
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
  }
  # Write batch files
  all_jobs <- paste0("# Run all jobs\n",paste(paste("sbatch", unlist(bash_path), "\n"), collapse = ""))
  writeLines(text = all_jobs, paste0(job_file_path, "/run_jobs.sh"))
  return(total_time)
}

calc_time_h <- function(h){
  res <- h
  paste0(ifelse(res < 10, paste0("0",as.character(res)), as.character(res)), ":00:00")
}
