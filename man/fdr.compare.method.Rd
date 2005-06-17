\name{fdr.compare.method}
\alias{fdr.compare.method}
\title{Comparison of the different analysis methods }
\description{
This function compares the different analysis methods for a given FDR threshold
}
\usage{
fdr.compare.method(data,qval=0.05,display.all=FALSE,lambda=seq(0,0.95,0.05))
}
\arguments{
  \item{data}{gene expression data object}
  \item{qval}{FDR cut-off}
  \item{display.all}{}
  \item{lambda}{Argument of the q-value function}
}
\details{}
\value{
A data frame with each gene classified as regulated/not-regulated in 
for each method (gene-specific, homoscedastic, Varmixt).
Also prints the tables of comparison.
}
\references{}
\author{Paul Delmar}
\note{}
\seealso{ \code{\link{fdr.compare.res}},\code{\link{compare.res}},\code{\link{vm.analysis}}, \code{\link{vm.analysis.paired}} }
\examples{}
\keyword{htest}
