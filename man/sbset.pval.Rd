\name{sbset.pval}
\alias{sbset.pval}

\title{Subset of vm.result object using gene p-values}
\description{This function finds and returns the subset of vm.result object using gene p-values}
\usage{
sbset.pval(data, pval, method = c("gene", "VM", "VM2", "anova")[2])
}

\arguments{
  \item{data}{gene expression data result object}
  \item{pval}{P value cut off}
  \item{method}{Method for computing the q-value. Must be "gene" or "VM" or "VM2" or "anova". }

}
\details{}
\value{A subset of the gene expression data object with the genes below a given
p value criterion.}
\references{}
\author{Paul Delmar}
\note{}
\seealso{}
\examples{}
\keyword{htest}
