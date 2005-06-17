\name{vm.analysis.step.2}
\alias{vm.analysis.step.2}
\title{Mixture model analysis}
\description{
This function performs the last stage of mixture model analysis

}
\usage{
vm.analysis.step.2(data, n.mixt = NULL, filename = NULL,crit=c("AIC","BIC")[2],stop.crit.1=1.e-6,
                             criterion.1=c("likelihood","parameter")[1],criterion.2=c("likelihood","parameter")[1],
                             stop.crit.2=1.e-8,display=TRUE)
}
\arguments{
  \item{data}{ a gene expression data object }
  \item{n.mixt}{number of components in the model}
  \item{filename}{optional output filename }
  \item{criterion.1}{either "likelihood" or "parameter". The criterion for stopping EM algorithm while
          trying to determine the number of variance groups}
  \item{stop.crit.1}{The stopping relative precision limit for stopping EM algorithm}
  \item{criterion.2}{Either "likelihood" or "parameter". The criterion for stopping EM algorithm while
          trying to estimate the parameters.}
  \item{stop.crit.2}{The stopping relative precision limit for stopping EM algorithm}
  \item{display}{Boolean. Should the result of model fitting be displayed on the screen on the fly ?}
}
\details{
}
\value{
}
\references{ }
\author{Paul Delmar and Julie Aubert}
\note{}
\seealso{\code{\link{vm.analysis}},\code{\link{vm.analysis.paired}} }
\examples{}
\keyword{internal}
