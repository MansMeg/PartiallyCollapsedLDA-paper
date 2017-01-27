#' Function to create bash files, config files and NSC Runs
#' 
#' @description
#'  Function to generate bash scripts and config files to run at NSC.
#'  The jar file assumes to be localed at the part_to_project.
#'  
#'  @param path_to_project Path to the project 
#'  @param java_seeds The different runs to use (one run for each seed)
#'  @param K The number of topic (vector)
#'  @param data_paths The datasets to use (vector) as path
#'  @param R_in_parallel Should the postprocessing in R use paralell processing?
#'  @param cores The number of cores to use (default is 8)
#'  @param mail_to e-mail adress to use for jobs
#'  @param delete_files Should the files be deleted after run?
#'  
#'  @export
create_nsc_runs_ineff <- function(path_to_project = "", java_seeds, K, data_paths, R_in_parallel=FALSE, cores = 8, mail_to, delete_files=FALSE){
#  data_paths <- c("datasets/kos.txt", "datasets/nips.txt")
#  K <- c(10, 20)

bash_script_file <- "#!/bin/bash
#
#SBATCH -J [actual_config]
#SBATCH -t [time_to_use]
#SBATCH --mem=[memory_to_use]
#SBATCH -n [cores_to_use]
#SBATCH --mail-type=ALL
#SBATCH --mail-user=[mail_to]
#
module load java/jre1.7.0_04
module load R/3.1.1
sleep [sleep_sec]
java -cp PCPLDA-1.0.2.jar tgs.runnables.InefficencyExperiment --run_cfg=[actual_config_path]
R CMD BATCH --vanilla '--args config=\"[actual_config]\" delete_files=[should_files_be_deleted] in_parallel=[do_in_parallel]' R/ineff_run_script.R results/[actual_config].Rout
"

config_file <- 
"configs = [actual_config]
no_runs = 1

[[actual_config]]
title = [actual_config]
description = Inefficiency experiment (data = [data] K = [K] seed = [seed])
dataset = [data]
scheme = collapsed
seed = [seed]
topics = [K]
alpha = 0.1
beta = 0.01
iterations = 10000
pcp_iterations = 2000
print_ndocs_interval = 1, 2000
print_ndocs_cnt = 1000
print_ntopwords_interval = 1, 2000
print_ntopwords_cnt = 1000
diagnostic_interval = -1
dn_diagnostic_interval = -1
batches = [cores_to_use]
rare_threshold = 10
topic_interval = 1000
start_diagnostic = 500
debug = 0

"

  # Create folder for job files
  dir.create(paste0(path_to_project, "NSC"))
  dir.create(paste0(path_to_project, "NSC/job_files"))
  dir.create(paste0(path_to_project, "NSC/job_files/ineff"))
  job_file_path <- paste0(path_to_project, "NSC/job_files/ineff")

  # Loop over K and data_sets
  bash_path <- time_vec <- list()
  sleep_sec <- 1
  cores_to_use <- cores
  should_files_be_deleted <- delete_files
  for (s in java_seeds){
    for (k in K){ 
      for (data_txt in data_paths){
        bash <- bash_script_file
        cfg <- config_file
        if(!file.exists(paste0(path_to_project, data_txt))) warning(paste0(paste0(path_to_project, data_txt), " do not exist."))
        data_name <- unlist(stringr::str_split(data_txt, "/|.txt"))
        data_name <- data_name[length(data_name) - 1]
        # K and data
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
                                         pattern = "\\[should_files_be_deleted\\]", 
                                         replacement = as.character(should_files_be_deleted))
        bash <- stringr::str_replace_all(string = bash, 
                                         pattern = "\\[do_in_parallel\\]", 
                                         replacement = as.character(R_in_parallel))
        bash <- stringr::str_replace_all(string = bash, 
                                         pattern = "\\[cores_to_use\\]", 
                                         replacement = as.character(cores_to_use))
        cfg <- stringr::str_replace_all(string = cfg, 
                                        pattern = "\\[cores_to_use\\]", 
                                        replacement = as.character(cores_to_use))
        bash <- stringr::str_replace_all(string = bash, 
                                         pattern = "\\[sleep_sec\\]", 
                                         replacement = as.character(sleep_sec))
        sleep_sec <- sleep_sec + 2
        # Actual config
        actual_config <- paste("ineff", data_name, k, "seed", s, sep="_")
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
        time_to_use <- calc_time_usage(k, data_name) 
        time_vec[[length(time_vec) + 1]] <- time_to_use
        
        mail_to <- "mons.magnusson@gmail.com"
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

  # Write batch files
  all_jobs <- paste0("# Run all jobs\n",paste(paste("sbatch", unlist(bash_path[order(unlist(time_vec), decreasing = TRUE)]), "\n"), collapse = ""))
  writeLines(text = all_jobs, paste0(job_file_path, "/run_jobs.sh"))
}

calc_memory_usage <- function(k, data_name){
  if (stringr::str_detect(string = data_name, "kos")) {res <- 8000}
  if (stringr::str_detect(string = data_name, "nips")) {res <- 8000}
  if (stringr::str_detect(string = data_name, "enron")) {res <- 16000}
  return(res)
}

calc_time_usage <- function(k, data_name){
  if (stringr::str_detect(string = data_name, "kos")) {res <- 1.5}
  if (stringr::str_detect(string = data_name, "nips")) {res <- 3.25}
  if (stringr::str_detect(string = data_name, "enron")) {res <- 8.25}
  res <- ceiling(res * k/10) + 2
  paste0(ifelse(res < 10, paste0("0",as.character(res)), as.character(res)), ":00:00")
}


#' Function to create bash files, config files and NSC Runs for RS experiment
#' 
#' @description
#'  Function to generate bash scripts and config files to run at NSC.
#'  The jar file assumes to be localed at the part_to_project.
#'  
#'  @inheritParams create_nsc_runs_ineff
#'  @param cores Vector with the number of cores to use
#'  @param samplers Vector with the different samplers to use
#'  @param samplers Vector with the different samplers to use
#'  
#'  @examples
#'  create_nsc_runs_rs(java_seeds = 20150326 + (0:5), 
#'                     K = c(50, 100), 
#'                     data_paths = c("datasets/enron.txt", "datasets/nips.txt"),
#'                     sub_samplers = c("full", "token_mass", "delta_n"),
#'                     full_phi_it = c(5,10),
#'                     percentage_phi=c(0.01, 0.05, 0.2, 0.5),
#'                     percentage_z=c(1, 0.5, 0.2),
#'                     mail_to = "mons.magnusson[at]gmail.com, leif.jonsson[at]ericsson.com")
#'                          
#'  @export
create_nsc_runs_rs <- function(path_to_project = "", 
                               java_seeds, 
                               K, 
                               data_paths, 
                               mail_to,
                               cores = 8, 
                               sub_samplers = c("standard", "delta_n", "token_mass"), 
                               full_phi_it = 1, 
                               percentage_phi=1.0,
                               percentage_z=1.0){
  #  data_paths <- c("datasets/enron.txt", "datasets/nips.txt")
  # sub_samplers "utils.randomscan.topic.PercentageTopicBatchBuilder"
  # K <- c(20, 100)
  # java_seeds <- 20150326
  # cores <- c(64, 32, 16, 8, 4, 1)
  # mail_to <- "leif.jonsson[at]ericsson.com"
  # samplers <- c("collapsed", "uncollapsed", "adlda")
  bash_script_file <- "#!/bin/bash
#
#SBATCH -J [actual_config]
#SBATCH -t [time_to_use]
#SBATCH -A liu-2014-00036-36
#SBATCH --mem=[memory_to_use]
#SBATCH -n [cores_to_use]
#SBATCH --mail-type=ALL
#SBATCH --mail-user=[mail_to]
#SBATCH --output=\"slurm-rs-%j.out\"
#
module load java/jre1.7.0_04
sleep [sleep_sec]
java -cp PCPLDA-1.0.4.jar tgs.runnables.RSExperiment --run_cfg=[actual_config_path]
"
  
  config_file <- "configs = [actual_config]
no_runs = 1
  
[[actual_config]]
title = [actual_config]
description = RS experiment ([actual_config])
dataset = [data]
scheme = uncollapsed
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
[rs_args]
debug = 0

"
  
  # Create folder for job files
  dir.create(paste0(path_to_project, "NSC"))
  dir.create(paste0(path_to_project, "NSC/job_files"))
  dir.create(paste0(path_to_project, "NSC/job_files/rs"))
  job_file_path <- paste0(path_to_project, "NSC/job_files/rs")
  
  # Loop over K, data_sets, sampler and cores
  bash_path <- list()
  total_time <- 0
  sleep_sec <- 1
  cores_no <- cores
  
  for (data_txt in data_paths){
    for (k in K){ 
      for (sub_sampler in sub_samplers){
        for (prcnt_z in seq_along(percentage_z)){
          for (prcnt_phi in seq_along(percentage_phi)){
          for (phi_it_i in seq_along(full_phi_it)){
           for (s in java_seeds){
              
              if(sub_sampler == "full"){
                if(prcnt_z > 1) next()
                if(prcnt_phi > 1) next()
                if(phi_it_i > 1) next()
                act_sub_cfg <- ""
                rs_args <- "batch_building_scheme = utils.randomscan.document.EvenSplitBatchBuilder\n  "
              } else if(sub_sampler == "delta_n") {
                if(prcnt_phi > 1) next()
                rs_args <- paste("topic_index_building_scheme = utils.randomscan.topic.DeltaNTopicIndexBuilder",
                                 paste0("full_phi_period = ", full_phi_it[phi_it_i]), 
                                 "batch_building_scheme = utils.randomscan.document.FixedSplitBatchBuilder",
                                 paste0("fixed_split_size_doc = ", percentage_z[prcnt_z]), 
                                 sep = "\n")
                act_sub_cfg <- paste("fullphi",full_phi_it[phi_it_i],"pctz", percentage_z[prcnt_z]*100, sep="_")
              } else if(sub_sampler == "token_mass") {
                rs_args <- paste("topic_index_building_scheme = utils.randomscan.topic.MandelbrotTopicIndexBuilder",
                                 paste0("full_phi_period = ", full_phi_it[phi_it_i]), 
                                 paste0("percent_top_tokens = ", percentage_phi[prcnt_phi]),  
                                 "batch_building_scheme = utils.randomscan.document.FixedSplitBatchBuilder",
                                 paste0("fixed_split_size_doc = ", percentage_z[prcnt_z]),
                                 sep = "\n")
                act_sub_cfg <- paste("fullphi",full_phi_it[phi_it_i],"pctz", percentage_z[prcnt_z]*100,"pctphi", percentage_phi[prcnt_phi]*100, sep="_")
              }
              
              bash <- bash_script_file
              cfg <- config_file
              cores_to_use <- cores_no
              cores_to_use_topic <- max(1, cores_no - 1)
              if(!file.exists(paste0(path_to_project, data_txt))) warning(paste0(paste0(path_to_project, data_txt), " do not exist."))
              data_name <- unlist(stringr::str_split(data_txt, "/|.txt"))
              data_name <- data_name[length(data_name) - 1]
              
              # K, data, sampler, cores, rs_args
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
                                              pattern = "\\[rs_args\\]", 
                                              replacement = rs_args)
              
              # Sleep
              bash <- stringr::str_replace_all(string = bash, 
                                               pattern = "\\[sleep_sec\\]", 
                                               replacement = as.character(sleep_sec))
              sleep_sec <- sleep_sec + 2

              # Actual config
              actual_config <- paste("rs", data_name, k, "samp",substr(sub_sampler,1,5), act_sub_cfg, "seed", s, sep="_")
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
              time_to_use <- calc_time_usage_rs(k, data_name, cores_to_use)
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
    if(k == 20){res <- 8} else if (k == 100) {res <- 20} 
  }
  paste0(ifelse(res < 10, paste0("0",as.character(res)), as.character(res)), ":00:00")
}




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
#'    create_nsc_runs_spalias(java_seeds=2000 + 1:5,
#'                            K=c(20,100),
#'                            data_paths = c("datasets/enron.txt", "datasets/nips.txt"),
#'                            mail_to="mons.magnusson[at]gmail.com"
#'                            )          
#'  @export
create_nsc_runs_spalias <- function(path_to_project = "", 
                               java_seeds, 
                               K, 
                               data_paths, 
                               mail_to,
                               cores = 8){
  samplers <- c("uncollapsed", "spalias")
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
#SBATCH --output=\"slurm-spalias-%j.out\"
#
module load java/jre1.7.0_04
sleep [sleep_sec]
java -cp PCPLDA-1.0.6.jar tgs.runnables.SpaliasExperiment --run_cfg=[actual_config_path]
"
  
  config_file <- "configs = [actual_config]
no_runs = 1

[[actual_config]]
title = [actual_config]
description = Spalias speedup ([actual_config])
dataset = [data]
scheme = [sampler]
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
debug = 0

"
  
  # Create folder for job files
  dir.create(paste0(path_to_project, "NSC"))
  dir.create(paste0(path_to_project, "NSC/job_files"))
  dir.create(paste0(path_to_project, "NSC/job_files/spalias"))
  job_file_path <- paste0(path_to_project, "NSC/job_files/spalias")
  
  # Loop over K, data_sets, sampler and cores
  bash_path <- list()
  total_time <- 0
  sleep_sec <- 1
  cores_no <- cores
  
  for (data_txt in data_paths){
    for (k in K){ 
      for (sampler in samplers){
        for (s in java_seeds){
          bash <- bash_script_file
          cfg <- config_file
          cores_to_use <- cores_no
          cores_to_use_topic <- max(1, cores_no - 1)
          if(!file.exists(paste0(path_to_project, data_txt))) warning(paste0(paste0(path_to_project, data_txt), " do not exist."))
          data_name <- unlist(stringr::str_split(data_txt, "/|.txt"))
          data_name <- data_name[length(data_name) - 1]
          
          # K, data, sampler, cores, rs_args
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
          
          # Sleep
          bash <- stringr::str_replace_all(string = bash, 
                                           pattern = "\\[sleep_sec\\]", 
                                           replacement = as.character(sleep_sec))
          sleep_sec <- sleep_sec + 2
          
          # Actual config
          actual_config <- paste("spalias", data_name, k, substr(sampler,1,5), "seed", s, sep="_")
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
  # Write batch files
  all_jobs <- paste0("# Run all jobs\n", paste(paste("sbatch", unlist(bash_path), "\n"), collapse = ""))
  writeLines(text = all_jobs, paste0(job_file_path, "/run_jobs.sh"))
  return(total_time)
}


calc_time_usage_spalias <- function(k, data_name, cores_to_use){
  if (stringr::str_detect(string = data_name, "kos")) {res <- 15}
  if (stringr::str_detect(string = data_name, "nips")) {
    if(k == 20){res <- 2} else if (k == 100) {res <- 4}
  }
  if (stringr::str_detect(string = data_name, "enron")) {
    if(k == 20){res <- 8} else if (k == 100) {res <- 20} else if (k == 200) {res <- 40}
  }

  paste0(ifelse(res < 10, paste0("0",as.character(res)), as.character(res)), ":00:00")
}

