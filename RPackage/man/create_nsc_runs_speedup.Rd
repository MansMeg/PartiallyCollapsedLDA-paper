% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nsc_speedup.R
\name{create_nsc_runs_speedup}
\alias{create_nsc_runs_speedup}
\title{Function to create bash files, config files and NSC Runs}
\usage{
create_nsc_runs_speedup(path_to_project = "", java_seeds, K, data_paths,
  cores, iterations = 20000, directory = "speedup", samplers, mem, mail_to,
  huge_node = TRUE)
}
\description{
Function to generate bash scripts and config files to run at NSC.
 The jar file assumes to be localed at the part_to_project.
 
 @inheritParams create_nsc_runs_ineff
 @param cores Vector with the number of cores to use
 @param samplers Vector with the different samplers to use
 @param huge_node Should the huge node be used?
 
 @examples
 create_nsc_runs_speedup(java_seeds = 20150326, 
                         cores = c(64, 32, 16, 8, 4, 1), 
                         K = c(20, 100), 
                         data_paths = c("datasets/enron.txt", "datasets/nips.txt"),
                         samplers = c("collapsed", "uncollapsed", "adlda"),
                         mail_to = "leif.jonsson[at]ericsson.com")
                         
 @export
}

