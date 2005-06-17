\name{choose.nmixt.VM}
\alias{choose.nmixt.VM}

\title{How to choose the number of components of the mixture model}
\description{A function to choose the number of  components in the mixture model}
\usage{
choose.nmixt.VM(data, crit = c("AIC", "BIC")[2], stop.crit = 1e-06, display = TRUE, criterion = c("likelihood", "parameter")[1])
}

\arguments{
  \item{data}{gene expression data object}
  \item{crit}{penality to use : "BIC" or "AIC"}
  \item{stop.crit}{Value of stopping criterion for EM algo}
  \item{display}{Boolean. Should results be displayed on the screen ?}
  \item{criterion}{"likelihood" or "parameter" : the criterion for stopping EM algo}
}
\details{}
\value{}
\references{ }
\author{Paul Delmar}
\note{}

\seealso{}
\examples{}
\keyword{internal}
