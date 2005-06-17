\name{choose.nmixt}
\alias{choose.nmixt}
\title{Fow to choose the number of components in the mixture model}
\description{
This function finds the number of components that minimizes the BIC criteria.
}
\usage{
choose.nmixt(VAR,df,crit=c("AIC","BIC")[2],stop.crit=stop.crit.2,display=TRUE,
                       criterion=c("likelihood","parameter")[1])
}

\arguments{
  \item{VAR}{Vector of estimated variance}
  \item{df}{degrees of freedom of the estimated variance}
  \item{crit}{method for choosing the number of component : "BIC" or "AIC"}
  \item{stop.crit}{stop criterion for EM algorithm}
  \item{display}{Boolean. Should the estimated parameters be displayed on the screen ?}
  \item{criterion}{"likelihood" or "parameter". The convergence of EM algo will be decided
                    with regard to the increase of likelihood or lack of noticeable in parameter value.}
}
\details{
}
\value{
}
\references{}
\author{Paul Delmar and Julie Aubert}
\note{}

 \seealso{\code{\link{compute.dif}},\code{\link{compute.dif.paired}},\code{\link{compute.BIC}}}
\examples{


}
\keyword{internal}
