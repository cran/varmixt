\name{qval.gene.vect}
\alias{qval.gene.vect}

\title{Vector of gene specific q-values from vm.result data object}
\description{This function extracts the vector of gene specific q-values from vm.result data object}
\usage{
qval.gene.vect(data, lambda = seq(0, 0.95, 0.05), pi0.meth = "smoother", fdr.level = NULL, robust = FALSE)
}

\arguments{
  \item{data}{ Gene expression data object }
  \item{lambda}{ parameter for qvalue function }
  \item{pi0.meth}{parameter for qvalue function }
  \item{fdr.level}{parameter for qvalue function}
  \item{robust}{logical, parameter for qvalue function }
}
\details{}
\value{The vector of q value computed from the p values of the gene specific method}
\references{}
\author{Paul Delmar}
\note{}

 

\seealso{}
\examples{}
\keyword{htest}
