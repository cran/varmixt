\name{compute.param}
\alias{compute.param}

\title{Computes the mixture parameters}
\description{
Computes the mixture parameters
}
\usage{
compute.param(VAR,df,nmixt,sd.init=NULL,pi.init=NULL,criterion=c("likelihood","parameter")[1],
                        stop.crit=stop.crit.2)
                        }

\arguments{
  \item{VAR}{Vector of estimated variance}
  \item{df}{degrees of freedom of the estimated variance}
  \item{nmixt}{Number of components in the mixture}
  \item{sd.init}{optional vector of initial standard deviation }
  \item{pi.init}{optional vector of initial probability}
  \item{criterion}{which type of criterion for the convergence of EM algorithm}
  \item{stop.crit}{Defines the convergence of EM algo (EM algo relative difference of criterion between two iterations is less than stop.crit)}
}
\details{

}
\value{
}
\references{}
\author{Paul Delmar}
\note{}



\seealso{ }
\examples{}
\keyword{internal}
