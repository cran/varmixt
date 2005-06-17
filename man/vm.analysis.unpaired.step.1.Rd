\name{vm.analysis.unpaired.step.1}
\alias{vm.analysis.unpaired.step.1}
\title{First step of analysis in unpaired data case}
\description{
First step of analysis in unpaired data case}
\usage{
vm.analysis.unpaired.step.1(geneId, cont, treat, gene.anot = NULL, badqual = NULL, qualtol = NULL, center = TRUE, loess.cor = FALSE, min.rep = 2)
}
\arguments{
  \item{geneId}{a vector of the vector of gene names  }
  \item{cont}{matrix of gene log-intensity in condition 1. The matrix has
                 one gene per row and one replicate per column }
  \item{treat}{matrix of gene log-intensity in condition 1  }
  \item{gene.anot}{data.frame with further gene anotations  }
  \item{badqual}{integer matrix of number of bad quality observation per gene and replicate  }
  \item{qualtol}{integer. Genes with more than qualtol bad quality observations are removed
                   from the analysis. This argument is ignored if Badqual is NULL  }
  \item{center}{Logical. If True each array is centered to have a 0 mean log-ratio }
  \item{loess.cor}{Logical. If True then loess transformation of the data is performed.
                    If False no loess transformation is performed  }
  \item{min.rep}{ Minimum number of non-missing value per gene and per condition. Must be at least 2 }
}
\details{
}
\value{
gene expression data object
}
\references{}
\author{Paul Delmar}
\note{}
\seealso{}
\examples{}
\keyword{internal}
