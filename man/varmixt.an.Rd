\name{varmixt.an}
\alias{varmixt.an}

\title{Mixture model on the variance}
\description{This function performs the mixture model on the
variance analysis.
}
\usage{
varmixt.an(data, nmixt,pi=NULL,b=NULL,ppost=NULL,c=NULL,deno=NULL,stop.crit.2,criterion)
}

\arguments{
  \item{data}{A gene expression data object}
  \item{nmixt}{The number of components in the mixture model}
  \item{pi}{Internal parameter for the mixture model. If NULL the parameter is computed by the function}
  \item{b}{Internal parameter for the mixture model. If NULL the parameter is computed by the function}
  \item{ppost}{Internal parameter for the mixture model. If NULL the parameter is computed by the function}
  \item{c}{Internal parameter for the mixture model. If NULL the parameter is computed by the function}
  \item{deno}{Internal parameter for the mixture model. If NULL the parameter is computed by the function}
  \item{stop.crit.2}{}
  \item{criterion}{}
}
\details{}
\value{A gene expression data object with the result of mixture model analysis}
\references{}
\author{Paul Delmar and Julie Aubert}
\note{}

\seealso{\code{\link{vm.analysis}}, \code{\link{vm.analysis.paired}}}
\examples{}
\keyword{internal}
