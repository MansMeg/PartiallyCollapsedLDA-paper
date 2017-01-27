# Install and load package
devtools::install_local(path = "RPackage")
library(pcplda)

# pcplda::random_order_corpus(input_file = "../datasets_pclda/pubmed.txt", output_file = paste0("../datasets_pclda/pubmed_random.txt"), seed = s)  
pcplda::subset_corpus(input_file = paste0("../datasets_pclda/pubmed_random.txt"), output_file = paste0("../datasets_pclda/pubmed_randomp10.txt"), prop = 0.1)

# Generate pubmed datasets
for(s in 4711:4716){
  pcplda::random_order_corpus(input_file = "../datasets_pclda/pubmed.txt", output_file = paste0("../datasets_pclda/pubmed_random",s,".txt"), seed = s)  
  pcplda::random_order_corpus(input_file = "../datasets_pclda/pubmed_randomp10.txt", output_file = paste0("../datasets_pclda/pubmed_random",s,"p10.txt"), seed = s)  
}
