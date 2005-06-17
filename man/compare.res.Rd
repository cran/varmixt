\name{compare.res}
\alias{compare.res}
\title{Compare the results from 2 analysis}
\description{
 Compare the results from 2 analysis with p-value criterion
}
\usage{
compare.res(data1, data2, pval = 0.05)
}

\arguments{
 \item{data1}{gene expression data object (results)}
  \item{data2}{gene expression data object (results)}
  \item{pval}{p-value cut-off}
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
\seealso{ \code{\link{fdr.compare.res}},\code{\link{compare.method}},\code{\link{vm.analysis}}, \code{\link{vm.analysis.paired}} }
\examples{}

\keyword{htest}
