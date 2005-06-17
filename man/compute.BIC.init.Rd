\name{compute.BIC.init}
\alias{compute.BIC.init}

\title{Computation of parameters of the mixture model given initial parameters}
\description{
This function computes parameters of the mixture model given initial parameters
}
\usage{
compute.BIC.init(VAR,df,sd.init,pi.init,nmixt,stop.crit=stop.crit.2,display=TRUE,PPOST=FALSE,
                           niter.max=50000,VAR.OUT=FALSE,criterion=criterion)}

\arguments{
  \item{VAR}{Vector of estimated variance}
  \item{df}{degrees of freedom of the estimated variance}
  \item{sd.init}{vector of initial standard deviation for the mixture}
  \item{pi.init}{vector of initial prior probability for the mixture}
  \item{nmixt}{Integer : number of components}
  \item{stop.crit}{ Real : convergence criteria for the EM algo}
  \item{display}{Boolean : Should the program write its results on the console ?}
  \item{PPOST}{Boolean : Should the programm export the matrix of posterior probability of each gene ?}
  \item{niter.max}{Integer : maximum number of iteration of the EM algorithm}
  \item{VAR.OUT}{Boolean : Should the program export the vector of variance and the vector of corresponding components ?}
  \item{criterion}{Criterion for the convergence of EM algo. Either "likelihood" for criterion based on loglikelihood or "parameter"} 
  }
\details{}
\value{}
\references{}
\author{Paul Delmar}
\note{}



\seealso{ }
\examples{}
\keyword{internal}% at least one, from doc/KEYWORDS
