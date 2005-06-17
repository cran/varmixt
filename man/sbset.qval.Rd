\name{sbset.qval}
\alias{sbset.qval}

\title{Subset of vm.result object using gene q-values}
\description{This function gets the subset of vm.result object using gene q-values}
\usage{
sbset.qval(data, qval, method = c("gene", "VM", "VM2", "anova")[2], lambda = seq(0, 0.95, 0.05))
}

\arguments{
  \item{data}{gene expression data result object}
  \item{qval}{FDR cut off}
  \item{method}{Method for computing the q-value. Must be "gene" or "VM" or "VM2" or "anova". }
  \item{lambda}{lambda parameter for computing the q value}
}
\details{}
\value{A subset of the gene expression data object with the genes below a given
q value criterion.}
\references{}
\author{Paul Delmar}
\note{}
\seealso{}
\examples{}
\keyword{htest}
