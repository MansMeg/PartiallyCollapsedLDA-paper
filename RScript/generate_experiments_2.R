# Install and load package
devtools::install_local(path = "RPackage")
library(pcplda)

# Generate PUBMED Experiment (16 cores)
# If 10 000 topics, 120 gB in memory is needed Else 30 gB is enough (not needed a fat node)

# Parameters
basic_account <- "xxx"
all_samplers <- c("spalias", "adlda", "lightpclda", "lightpcldaw2", "lightcollapsed")
data_sets <- paste0("pubmed_random", 4711:4715)
data_sets <- paste0("../datasets_pclda/", c(data_sets, paste0(data_sets, "p10")), ".txt")


# PubMed algo compare experiment
create_nsc_runs_pubmed(java_seeds = 4711,
                       time_limit_h = 25,
                       folder="pubmed_algo_experiment",
                       K = c(10, 100, 1000, 10000),
                       mem = 32000,
                       cores = 16,
                       data_paths = data_sets,
                       tfidf_vocab_size = rep(50000, 10),
                       samplers = all_samplers,
                       alpha_beta_prior = c(0.1, 0.01), 
                       huge_node = FALSE,
                       account = basic_account,
                       mail_to = "mons.magnusson@gmail.com")

# PubMed weak parallelism experiment (16-64 cores)
create_nsc_runs_pubmed(java_seeds = 4711, 
                       time_limit_h = 25,
                       folder="pubmed_weak_experiment",
                       K = c(10, 100, 1000, 10000),
                       mem = 32000,
                       cores = c(64, 32, 16),
                       data_paths = data_sets[1],
                       tfidf_vocab_size = 50000,
                       samplers = all_samplers,
                       alpha_beta_prior = c(0.1, 0.01), 
                       huge_node = TRUE,
                       account = "snic2016-1-307",
                       mail_to = "mons.magnusson@gmail.com")

# Variable selection experiment
create_nsc_runs_vs(java_seeds = 4711, 
                   K = c(100), 
                   folder="vs2",
                   data_paths = c("../datasets_pclda/nips_random.txt", data_sets[6]),
                   mem = 32000,
                   h = c(10, 48),
                   account = basic_account,
                   pis = c(1.0, 0.5, 0.1, 0.01, 0.001),
                   mail_to = "mons.magnusson@gmail.com",
                   iterations = 25000)

create_nsc_runs_vs(java_seeds = 4711, 
                   K = c(100), 
                   folder="vs3",
                   data_paths = c("../datasets_pclda/nips_random.txt"),
                   mem = 32000,
                   h = c(48),
                   account = basic_account,
                   pis = c(0.1, 0.01, 0.001),
                   mail_to = "mons.magnusson@gmail.com",
                   iterations = 250000)

# Stochastic VB data experiment
create_nsc_runs_pubmed(java_seeds = 4711:4715,
                       time_limit_h = 25,
                       folder="stochvb_experiment",
                       K = c(100),
                       mem = 32000,
                       cores = 16,
                       data_paths = c("../datasets_pclda/wiki_random.txt", "../datasets_pclda/nyt_random.txt"),
                       max_doc_buf_size = c("100000", "10000"),
                       tfidf_vocab_size = c(7700, 8000),
                       samplers = all_samplers,
                       alpha_beta_prior = c(0.1, 0.01), 
                       huge_node = FALSE,
                       account = basic_account,
                       mail_to = "mons.magnusson@gmail.com")

# Bad sorted data
create_nsc_runs_pubmed(java_seeds = 4711:4720,
                       folder="pubmed10_baddata_experiment",
                       K = c(100),
                       mem = 32000,
                       cores = 16,
                       iterations = 1000,
                       data_paths = c("../datasets_pclda/pubmed_random_10.txt", "../datasets_pclda/pubmed_bad_10_K100_sort.txt", "../datasets_pclda/pubmed_bad_10_K100_strat.txt"),
                       rare_word_limits = rep(10, 3),
                       samplers = c("spalias", "adlda"),
                       alpha_beta_prior = c(0.1, 0.01), 
                       huge_node = FALSE,
                       account = basic_account,
                       mail_to = "mons.magnusson@gmail.com")


# Check runs
# Check that the same initial log likelihood is calculated
create_nsc_runs_pubmed(java_seeds = 4711:4712,
                       folder="check_runs",
                       K = c(20, 100),
                       mem = 32000,
                       iterations = 20000,
                       cores = 16,
                       data_paths = "../datasets_pclda/nips_random.txt",
                       rare_word_limits = 10,
                       samplers = c("spalias", "adlda", "lightpclda", "lightpcldaw2", "lightcollapsed"),
                       alpha_beta_prior = c(0.1, 0.01), 
                       huge_node = FALSE,
                       account = basic_account,
                       mail_to = "mons.magnusson@gmail.com")


warnings()
