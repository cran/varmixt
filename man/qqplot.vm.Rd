\name{qqplot.vm}
\alias{qqplot.vm}
\title{QQ plot of the residuals}
\description{
The function plots a global qqplots of all residuals and
qqplots of residuals in each variance group
}
\usage{
qqplot.vm(data, ...)
}

\arguments{
  \item{data}{Gene expression data object. The result of vm.analysis,vm.analysis.2.Var or vm.analysis.paired function}
  \item{\dots}{other arguments to the qqnorm function}
}
\details{
}
\value{
a global qqplots of all residuals and
qqplots of residuals in each variance group
}
\references{}
\author{Paul Delmar}
\note{}

\seealso{\code{\link{vm.analysis}}, \code{\link{vm.analysis.paired}},
        \code{\link{plotrm}}, \code{\link{plotvm}}, \code{\link{boxplotvm}}}
 }
\examples{}
\keyword{htest}
