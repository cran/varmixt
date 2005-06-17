\name{build.data}
\alias{build.data}
\title{How to build gene expression data object wiht unpaired data}
\description{
This function performs simple operations to build an object for further processing with analysis functions
}
\usage{
build.data(name,cond1,cond2,badqual=NULL,gene.anot=NULL,min.rep=2)
}
\arguments{
    \item{name}{vector with the identifier of each gene (one gene per row).}
    \item{cond1}{matrix of gene log-intensity in condition 1. The matrix has 
                 one gene per row and one replicate per column}
    \item{cond2}{matrix of gene log-intensity in condition 1}
    \item{badqual}{Optional argument : integer matrix of number of bad quality observation per gene and replicate.
                   Do not use badqual with different number of replicates in each condition}
    \item{gene.anot}{Optional arguments (vector, data.frame, matrix..) with extra annotation on the genes}
    \item{min.rep}{Minimum number of non-missing value per gene and per condition. Must be at least 2.}
} 
\value{A gene expression data object for further processing}
\author{Paul Delmar}

\keyword{internal}
