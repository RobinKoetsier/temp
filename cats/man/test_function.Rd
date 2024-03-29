% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_function.R
\name{test_function}
\alias{test_function}
\title{Load a Matrix}
\usage{
test_function(text = "Hello World!")
}
\arguments{
\item{infile}{Path to the input file}
}
\value{
A matrix of the infile
}
\description{
This function loads a file as a matrix. It assumes that the first column
contains the rownames and the subsequent columns are the sample identifiers.
Any rows with duplicated row names will be dropped with the first one being
kepted.
}
