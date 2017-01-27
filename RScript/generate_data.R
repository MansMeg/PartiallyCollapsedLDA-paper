# Generate datasets from pubmed experiments
devtools::install_local(path = "RPackage")
library(pcplda)

random_order_corpus(input_file = "datasets/pubmed.txt", output_file = "datasets/pubmed_random.txt", have_enough_memory = FALSE)

subset_corpus(input_file = "datasets/pubmed_random.txt", output_file = "datasets/pubmed_random_10.txt", prop = 0.1)
subset_corpus(input_file = "datasets/pubmed_random.txt", output_file = "datasets/pubmed_random_1.txt", prop = 0.01)

# Calculate rare word limits
calculate_rareword(corpus = "datasets/pubmed_random.txt", have_enough_memory = FALSE)
calculate_rareword(corpus = "datasets/pubmed_random_10.txt")
calculate_rareword(corpus = "datasets/pubmed_random_1.txt")

# Order NIPS and PUBMED 10% data by topics
order_data_by_topic_proportions(input_crp = "../datasets_pclda/nips_random.txt", 
                                topic_prop = "../PartiallyCollapsedLDA/Runs/RunSuite2016-04-13--16_43_47/Run2016-04-13--16_43_47/Generate_NIPS_BadData_20/nips_20_mean.csv", 
                                output_crp = "../datasets_pclda/nips_bad_K20.txt")

order_data_by_topic_proportions(input_crp = "../datasets_pclda/nips_random.txt", 
                                topic_prop = "../PartiallyCollapsedLDA/Runs/RunSuite2016-04-13--16_05_49/Run2016-04-13--16_05_49/Generate_NIPS_BadData/nips_100_mean.csv", 
                                output_crp = "../datasets_pclda/nips_bad_K100.txt")

order_data_by_topic_proportions(input_crp = "../datasets_pclda/pubmed_random_10.txt", 
                                topic_prop = "results_20160416/PCLDA/Runs/RunSuite2016-04-16--08_08_41/Run2016-04-16--08_08_41/Generate_PubMed_BadData/pubmed_100_mean.csv", 
                                output_sort_crp = "../datasets_pclda/pubmed_bad_10_K100_sort.txt",
                                output_strat_crp = "../datasets_pclda/pubmed_bad_10_K100_strat.txt",
                                batches=16)

# Create wikipedia corpus
wiki_folder <- "datasets/tagged.en/"
tmp_folder <- "datasets/tmp/"
mallet_file <- "datasets/wiki.txt"
mallet_file_random <- "datasets/wiki_random.txt"

parse_wiki_corpus(wiki_folder = wiki_folder, tmp_folder=tmp_folder, mallet_file = mallet_file)
random_order_corpus(input_file = mallet_file, output_file = mallet_file_random, have_enough_memory = TRUE)
