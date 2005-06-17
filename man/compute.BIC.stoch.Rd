\name{compute.BIC.stoch}
\alias{compute.BIC.stoch}

\title{Computes parameters of the mixture on the variance with EM algorithm with random initial parameters}
\description{
This function computes parameters of the mixture with random initial parameters
}
\usage{
compute.BIC.stoch(VAR,df,nmixt,stop.crit = stop.crit.2,seed = NULL,criterion = criterion,PPOST = FALSE)
}


\arguments{
  \item{VAR}{Vector of estimated variance}
  \item{df}{degrees of freedom of the estimated variance}
  \item{nmixt}{Integer : number of components}
  \item{stop.crit}{ Real : convergence criterion for the EM algo}
  \item{seed}{Integer : Seed for random number generator}
  \item{criterion}{Criterion for the convergence of EM algo}
  \item{PPOST}{Boolean : Should the programm export the matrix of posterior probability of each gene}
}
\details{}
\value{}
\references{}
\author{Paul Delmar}
\note{}

\seealso{}
\examples{}
\keyword{internal}
