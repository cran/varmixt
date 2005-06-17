\name{pval.an}
\alias{pval.an}

\title{Number of regulated genes}
\description{This function returns the number of regulated genes for a given 
significance level.
}
\usage{
pval.an(data,P=.05,corrected=FALSE,display=TRUE)
}

\arguments{
  \item{data}{Gene expression data object}
  \item{P}{the p-value criteria}
  \item{corrected}{If TRUE uses bonferonni corrected p-values.}
  \item{display}{}
}
\details{}
\value{
  a data frame with the number of genes regulated with the different
  method at the given significance (p-value) level.
}
\references{}
\author{Paul Delmar}
\note{}

\seealso{\code{\link{vm.analysis}}, \code{\link{vm.analysis.paired}}, \code{\link{fdr.an}}}
\examples{
}
\keyword{htest}
