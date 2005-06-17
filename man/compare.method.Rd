\name{compare.method}
\alias{compare.method}
\title{Compare the different analysis methods }
\description{
Compare the different analysis methods for a given p-value threshold
}
\usage{
compare.method(data, pval = 0.05)
}
\arguments{
  \item{data}{gene expression data object}
  \item{pval}{p-value cut-off}
}
\details{}
\value{
A data frame with each gene classified as regulated/not-regulated in 
for each method (gene-specific, homoscedastic, Varmixt).
Also prints the tables of comparison.}
\references{}
\author{Paul Delmar}
\note{}
\seealso{ \code{\link{fdr.compare.method}},\code{\link{compare.res}},\code{\link{vm.analysis}}, \code{\link{vm.analysis.paired}} }
\examples{}
\keyword{htest}
