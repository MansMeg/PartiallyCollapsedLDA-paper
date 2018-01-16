[![DOI](https://zenodo.org/badge/79641225.svg)](https://zenodo.org/badge/latestdoi/79641225)

# PartiallyCollapsedLDA-paper

Code to run experiments and compute results for the paper "Sparse Partially Collapsed MCMC for Parallel Inference in Topic Models"

Build the R package to get the functionality to produce experiments and do computations. 

The code in RScript/generate_experiments_X.R is generating bash-files to run on a slurm based computer cluster. See MansMeg/slurmr for a convinience package to create sbatch files.

To generate random datasets based on PUBMED, NIPS and ENRON see the code in:
RScript/generate_data.R

In this repository results used to produce the graphs can be found in Rscript/generate_figures.R

The results are included in the results.zip file that need to be extracted to produce the results.

If you have any questions, feel free to send an e-mail to mans.magnusson@liu.se.

