\name{compute.BIC}
\alias{compute.BIC}

\title{Computation of the parameters of the mixture with deterministic initial parameters
}
\description{
This function computes parameters of the mixture with deterministic initial parameters
}
\usage{
compute.BIC(VAR,df,nmixt,stop.crit=1.e-08,display=TRUE,criterion=c("likelihood","parameter")[2],
                      PPOST=FALSE,VAR.OUT=FALSE)
}

\arguments{
  \item{VAR}{Vector of estimated variance}
  \item{df}{degrees of freedom of the estimated variance}
  \item{nmixt}{Integer : number of components}
  \item{stop.crit}{ Real : convergence criteria for the EM algo}
  \item{display}{Boolean : Should the program write its results on the console ?}
  \item{criterion}{Criterion for the convergence of EM algo. Either "likelihood" : for criterion based on loglikelihood or "parameter"}
  \item{PPOST}{Boolean : Should the programm export the matrix of posterior probability of each gene?}
  \item{VAR.OUT}{Boolean : Should the program export the vector of variance and the vector of corresponding components?}

}
\details{}
\value{}
\references{}
\author{Paul Delmar and Julie Aubert}
\note{}
\seealso{}
\examples{}
\keyword{internal}
