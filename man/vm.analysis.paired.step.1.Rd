\name{vm.analysis.paired.step.1}
\alias{vm.analysis.paired.step.1}
\title{First step of analysis in paired data case}
\description{
Function to do the first step of analysis in paired data case}
\usage{
vm.analysis.paired.step.1(geneId, ratio, meanint = NULL, gene.anot = NULL, badqual = NULL, qualtol = NULL, center = TRUE, loess.cor = FALSE, min.rep = 2)
}
\arguments{
  \item{geneId}{a vector of the vector of gene names }
  \item{ratio}{a matrix of the normalized log-ratio}
  \item{meanint}{a matrix of the raw mean log-intensity}
  \item{gene.anot}{  }
  \item{badqual}{integer matrix of number of bad quality observation per gene and replicate.
                   Do not use badqual with different number of replicates in each condition.}
  \item{qualtol}{ integer. Genes with more than qualtol bad quality observations are removed
                   from the analysis. This argument is ignored is Badqual is NULL }
  \item{center}{Logical. If True each array*condition is centered to have a 0 mean log-intensity.  }
  \item{loess.cor}{ Logical. if True then loess transformation of the data is performed.
                    If False no loess transformation is performed }
  \item{min.rep}{ Minimum number of non-missing value per gene and per condition. Must be at least 2. }
}
\details{
}
\value{ A gene expression data object}
\references{}
\author{Paul Delmar}
\note{}


\seealso{}
\examples{}
\keyword{internal}
