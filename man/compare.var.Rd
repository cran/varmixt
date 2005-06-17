\name{compare.var}
\alias{compare.var}
\title{How to compare variance in the 2 conditions}
\description{This function compares variance in the 2 conditions}
\usage{
compare.var(data)}
\arguments{
  \item{data}{gene expression data object}
}
\details{}
\value{
 A data.frame with the columns :
  \item{var.treat}{gene variance estimate in the "treated" condition}
  \item{var.cont}{gene variance estimate in the "control" condition}
  \item{var.ratio}{gene variances ratio}
  \item{var.ratio.pval}{p-value (H0 is \{ratio==1\})}
}
\references{}
\author{Paul Delmar}
\note{}
\examples{}
\keyword{htest}
