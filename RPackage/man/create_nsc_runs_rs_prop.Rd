% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nsc_rs_prop.R
\name{create_nsc_runs_rs_prop}
\alias{create_nsc_runs_rs_prop}
\title{Function to create bash files, config files and NSC Runs for RS-prop experiment}
\usage{
create_nsc_runs_rs_prop(path_to_project = "", java_seeds, K, data_paths,
  mail_to, mem, cores = 8, step_sizes)
}
\description{
Function to generate runs for random scan tokens proportional to size.
 
 @inheritParams create_nsc_runs_ineff
 @param cores Vector with the number of cores to use
 @param samplers Vector with the different samplers to use
 @param samplers Vector with the different samplers to use
 
 @examples
 create_nsc_runs_rs_prop(java_seeds = 20150326 + (0:4), 
                    K = c(20, 100), 
                    data_paths = c("datasets/enron.txt", "datasets/nips.txt"),
                    step_sizes = c(1, 100, 200, 500, 1000, 2000),
                    mail_to = "mons.magnusson[at]gmail.com, leif.jonsson[at]ericsson.com")
                         
 @export
}

