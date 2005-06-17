\name{compute.param.VM}
\alias{compute.param.VM}

\title{Computes the parameters given the number of components in the model}
\description{Computes the parameters given the number of components in the model
}
\usage{
compute.param.VM(data, nmixt, sd.init = NULL, pi.init = NULL, criterion = c("likelihood", "parameter")[1], stop.crit = 1e-08)
}

\arguments{
  \item{data}{ gene expression data object}
  \item{nmixt}{ number of variance groups }
  \item{sd.init}{ vector of starting value of groups standard deviation }
  \item{pi.init}{ vector of starting value of relative abundance of group }
  \item{criterion}{"likelihood" or "parameter" : Stopping criterion for the EM parameter}
  \item{stop.crit}{Value of the stopping parameter}
}
\details{}
\value{}
\references{ }
\author{Paul Delmar}
\note{}

\seealso{}
\examples{}
\keyword{internal}
