\name{find.qval.index}
\alias{find.qval.index}

\title{Subset of genes based on Q value}
\description{This function extracts a subset of genes based on Q value}
\usage{
find.qval.index(data, qval, method = c("gene", "VM", "VM2", "anova")[2], lambda = seq(0, 0.95, 0.05))
}
\arguments{
  \item{data}{gene expression data object}
  \item{qval}{qvalue cut-off}
  \item{method}{analysis method}
  \item{lambda}{qvalue function parameter}
}
\details{}
\value{}
\references{}
\author{Paul Delmar}
\note{}
\seealso{}
\examples{}
\keyword{htest}
