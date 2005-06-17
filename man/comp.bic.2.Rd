\name{comp.bic.2}
\alias{comp.bic.2}
\title{Study of the evolution of log-likelihood}
\description{
Study of the evolution of log-likelihood given the number of components in the model
}
\usage{
comp.bic.2(VAR, df, stop.crit = 1e-06, n.max = 20)
}
\arguments{
  \item{VAR}{Vector of estimated variance}
  \item{df}{degrees of freedom of the estimated variance}
  \item{stop.crit}{ Real : convergence criterion for the EM algo}
  \item{n.max}{Maximum number of mixtures to consider}
}
\details{
}
\value{
the difference between the log-likelihood with k and 1 component
}
\references{}
\author{Paul Delmar }
\note{}


\examples{
}
\keyword{internal}
