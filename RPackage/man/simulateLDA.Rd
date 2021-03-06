% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulateLDA.R
\name{simulateLDA}
\alias{simulateLDA}
\title{simulateLDA}
\usage{
simulateLDA(K = 5, V = 1000, D = 1000, mu_Nd = 100, alphaPhi = 0.01,
  alphaTheta = 0.1, seed = NULL)
}
\arguments{
\item{K}{The number of topics}

\item{V}{The vocabulary size (1000 is default)}

\item{D}{The number of documents/observations (5000 is default)}

\item{mu_Nd}{The mean number of poissondistributed words per document (500 is default)}

\item{alphaPhi}{The dirichlet parameter for Phi (0.01 is default)}

\item{alphaTheta}{The dirichlet parameter for Theta (0.1 is default)}

\item{seed}{The random seed to use to simulate dataset.}
}
\value{
A list with simulated data and parameters.
}
\description{
Function to simulate data from a LDA model
}

