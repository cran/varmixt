\name{build.data.paired}
\alias{build.data.paired}
\title{How to build a gene expression data object with paired data}
\description{
Performs simple operation to build an object for further processing with analysis functions
}
\usage{
build.data.paired(name, ratio, meanint, badqual = NULL, gene.anot = NULL,min.rep=2)
}

\arguments{
  \item{name}{vector with the identifier of each gene (one gene per row).}
  \item{ratio}{matrix of gene log-ratio. The matrix has 
                 one gene per row and one replicate per column}
  \item{meanint}{matrix of gene log-intensity}
  \item{badqual}{Optional argument : integer matrix of number of bad quality observation per gene and replicate.
                   Do not use badqual with different number of replicates in each condition}
  \item{gene.anot}{Optional arguments (vector, data.frame, matrix..) with extra annotation on the genes}
  \item{min.rep}{Minimum number of non-missing value per gene and per condition. Must be at least 2.}
}
\details{

}
\value{ A gene expression data object for futher processing
}
\references{}
\author{Paul Delmar}
\note{}

\seealso{}
\examples{}
\keyword{internal}
