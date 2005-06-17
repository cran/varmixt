\name{find.pval.index}
\alias{find.pval.index}

\title{Index of genes below using p-value criterion}
\description{This function gets the index of genes below using p-value criterion}
\usage{
find.pval.index(data, pval, method = c("gene", "VM", "VM2", "anova")[2])
}

\arguments{
  \item{data}{gene expression result data object}
  \item{pval}{p-value cut-off}
  \item{method}{string in "gene", "VM", "VM2", "anova". Method for computing the p-value  }
}
\details{}
\value{A vector with the index of the genes below a given p-value cut-off}
\references{}
\author{Paul Delmar}
\note{}
\seealso{}
\examples{}
\keyword{internal}
