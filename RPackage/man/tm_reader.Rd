% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tm_reader.R
\name{tm_reader}
\alias{tm_reader}
\title{Read in text documents as VCorpus objects}
\usage{
tm_reader(txt_file)
}
\arguments{
\item{txt_file}{Text in "our" format to read in}
}
\description{
Read in text documents as VCorpus objects
}
\examples{
\dontrun{
 nips_corpus <- tm_reader("PCLDA_Data/nips.txt")
 enron_corpus <- tm_reader("PCLDA_Data/enron.txt")
}
}

