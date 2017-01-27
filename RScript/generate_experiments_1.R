# Inefficiency experiment
create_nsc_runs_ineff(K = c(10, 20, 50, 100), 
                      java_seeds = c(4711, 4712), 
                      data_paths = c("datasets/kos_random.txt", "datasets/nips_random.txt", "datasets/enron_random.txt"),
                      cores = 2,
                      R_in_parallel = FALSE,
                      delete_files = FALSE, 
                      mail_to = "mons.magnusson@gmail.com")

# Speedup experiment
create_nsc_runs_speedup(java_seeds = c(20150327,20150328), 
                        cores = c(64, 32, 16, 8, 4, 1), 
                        K = c(20, 100), 
                        data_paths = c("datasets/enron_random.txt", "datasets/nips_random.txt"),
                        samplers = c("collapsed", "uncollapsed", "adlda"),
                        mail_to = "mons.magnusson@gmail.com,leif.jonsson@ericsson.com",
                        huge_node=TRUE)

# Extra run I:
create_nsc_runs_speedup(java_seeds = 20150329:20150331, 
                        cores = c(64, 32, 16, 8, 4, 1), 
                        K = c(20, 100), 
                        data_paths = c("datasets/enron_random.txt", "datasets/nips_random.txt"),
                        samplers = c("collapsed", "adlda"),
                        mail_to = "mons.magnusson@gmail.com,leif.jonsson@ericsson.com",
                        huge_node=TRUE)

# Extra run II (Spalias):
create_nsc_runs_speedup(java_seeds = 20150327:20150331, 
                        cores = c(64, 32, 16, 8, 4, 1), 
                        K = c(20, 100), 
                        data_paths = c("datasets/enron_random.txt", "datasets/nips_random.txt"),
                        samplers = c("spalias"),
                        mail_to = "mons.magnusson@gmail.com,leif.jonsson@ericsson.com",
                        huge_node=TRUE)



# Random scan experiments
create_nsc_runs_rs(java_seeds = 2001 + (2:4), 
                     K = c(20, 100), 
                     data_paths = c("datasets/enron_random.txt", "datasets/nips_random.txt"),
                     sub_samplers = c("full", "token_mass", "delta_n"),
                     full_phi_it = c(5,10),
                     percentage_phi=c(0.01, 0.05, 0.2, 0.5),
                     percentage_z=c(1.0, 0.5, 0.2),
                     mail_to = "mons.magnusson@gmail.com")

# Rare word experiment
create_nsc_runs_rarewords(java_seeds=20150326 + 0:2,
                          K=c(20,100),
                          data_paths = c("datasets/enron_random.txt", "datasets/nips_random.txt"),
                          rare_words = list(c(14,43,104,193), c(12,30,62,108)),
                          mail_to="mons.magnusson@gmail.com")


# Spalias experiments
create_nsc_runs_spalias(java_seeds=2000 + 1:5,
                        K=c(20,100),
                        data_paths = c("datasets/enron_random.txt", "datasets/nips_random.txt"),
                        mail_to="mons.magnusson@gmail.com"
                        )


# RS proportional experiment
create_nsc_runs_rs_prop(java_seeds = 20150326 + 0:4, 
                        K = c(20, 100, 1000), 
                        data_paths = c("datasets/enron.txt", "datasets/nips.txt"),
                        step_sizes = c(1, 100, 200, 500, 1000),
                        mail_to = "mons.magnusson@gmail.com")


# AD-LDA mode experiments
create_nsc_runs_adlda_mode(java_seeds = c(20150326:20150331,20150401:20150404) , 
                      cores = c(8, 4, 2, 1), 
                      iterations = 5000,
                      K = c(20, 100), 
                      data_paths = c("datasets/nips.txt", "datasets/enron.txt"),
                      mail_to = "mons.magnusson@gmail.com")

# PubMed experiments
create_nsc_runs_pubmed(java_seeds = 20150326, 
                       cores = c(64, 32, 16), 
                       K = c(200, 1000),
                       iterations = 1000,
                       data_paths = "datasets/pubmed.txt",
                       samplers = c("spalias", "adlda"),
                       mail_to = "mons.magnusson@gmail.com")


create_nsc_runs_prior(java_seeds = 20150326, 
                        K = c(100, 200, 400), 
                        data_paths = "datasets/enron_random.txt",
                        samplers = c("spalias", "adlda"),
                        alpha = c(0.1, 0.01),
                        beta = c(0.1, 0.01),
                        mem = 24000,
                        h=8,
                        mail_to = "mons.magnusson@gmail.com")

# Run prior II
create_nsc_runs_prior(java_seeds = 20150326, 
                        K = c(100, 1024), 
                        data_paths = "datasets/enron_random.txt",
                        samplers = c("spalias", "adlda"),
                        alpha = 0.01,
                        beta = 0.01,
                        mem = 24000,
                        h=10,
                        mail_to = "mons.magnusson@gmail.com")




## New extra experiments

# Spalias speedup:
create_nsc_runs_speedup(java_seeds = 20150327:20150331, 
                        cores = c(64, 32, 16, 8, 4, 1), 
                        K = c(20, 100), 
                        data_paths = c("datasets/enron_random.txt", "datasets/nips_random.txt"),
                        samplers = c("spalias"),
                        mail_to = "mons.magnusson@gmail.com, leif.jonsson@ericsson.com",
                        mem= 64000,
                        huge_node=TRUE)

# RS proportional experiment
create_nsc_runs_rs_prop(java_seeds = 4711:4716, 
                        K = c(100, 1000, 2000), 
                        data_paths = c("datasets/enron_random.txt"),
                        step_sizes = c(1, 100, 200, 500, 1000),
                        mem=20000,
                        mail_to = "mons.magnusson@gmail.com")


# AD-LDA mode experiments + Phi computing times 
create_nsc_runs_adlda_mode(java_seeds = 4711:4721, 
                      cores = c(8, 4, 2, 1), 
                      samplers = c("spalias", "adlda"),
                      K = c(20, 100), 
                      mem=20000,
                      data_paths = c("datasets/nips.txt", "datasets/enron.txt"), # A random shuffle of each dataset is done
                      mail_to = "mons.magnusson@gmail.com")

# PubMed experiments
create_nsc_runs_pubmed(java_seeds = 4711, 
                       cores = c(64, 32, 16), 
                       K = c(100, 1000),
                       mem = 64000,
                       data_paths = "datasets/pubmed_random.txt",
                       samplers = c("spalias", "adlda"),
                       mail_to = "mons.magnusson@gmail.com")

# Prior Experiment
create_nsc_runs_prior(java_seeds = 4711:4716, 
                        K = c(100, 1000), 
                        data_paths = "datasets/enron_random.txt",
                        samplers = c("spalias", "adlda"),
                        alpha = c(0.1, 0.01),
                        beta = c(0.1, 0.01),
                        mem = 24000,
                        h=8,
                        mail_to = "mons.magnusson@gmail.com")

# VS experiments
create_nsc_runs_vs(java_seeds = 4711, 
                     K = c(20, 100), 
                     data_paths = c("datasets/enron_random.txt", "datasets/nips_random.txt"),
                     mem = 24000,
                     pis = c(0.5, 0.1, 0.01, 0.0001),
                     mail_to = "mons.magnusson@gmail.com")


# PubMed experiments extra
create_nsc_runs_pubmed(java_seeds = 4711, folder="pubmed_job_steal",
                       cores = c(16), 
                       K = c(100, 1000),
                       mem = 64000,
                       data_paths = "datasets/pubmed_random.txt",
                       samplers = c("spalias"),
                       mail_to = "mons.magnusson@gmail.com")


# Speedup experiment extra
create_nsc_runs_speedup(java_seeds = 4711:4713, 
                        cores = c(64, 32, 16, 8, 4, 1), 
                        K = c(20, 100), 
                        directory = "speedup_extra",
                        iterations = 7000,
                        data_paths = c("datasets/enron_random.txt", "datasets/nips_random.txt"),
                        samplers = c("spalias"),
                        mail_to = "mons.magnusson@gmail.com",
                        mem= 64000,
                        huge_node=TRUE)

