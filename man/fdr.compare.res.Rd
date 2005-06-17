\name{fdr.compare.res}
\alias{fdr.compare.res}
\title{Comparison of the results from 2 analysis}
\description{
 This function compares the results from 2 analysis with FDR criterium
}
\usage{
fdr.compare.res(data1,data2,qval.1=0.05,qval.2=qval.1,display.all=FALSE,lambda=seq(0,0.95,0.05))
}
\arguments{
  \item{data1}{gene expression data object (results)}
  \item{data2}{gene expression data object (results)}
  \item{qval.1}{FDR cut-off for data1}
  \item{qval.2}{FDR cut-off for data2}
  \item{display.all}{ }
  \item{lambda}{ }
}
\details{}
\value{
A data frame with each gene classified as regulated/not-regulated in 
the two data-set for each method (gene-specific, homoscedastic, Varmixt).
Also prints the tables of comparison.
}
\references{}
\author{Paul Delmar}
\note{}

\seealso{\code{\link{fdr.compare.method}},\code{\link{compare.res}},\code{\link{vm.analysis}}, \code{\link{vm.analysis.paired}} }
\examples{}
\keyword{htest}
