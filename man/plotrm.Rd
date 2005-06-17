\name{plotrm}
\alias{plotrm}

\title{Plot of mean log-ratio versus mean log-intensity}
\description{Plot of mean log-ratio versus mean log-intensity}
\usage{
plotrm(x, P = 0.05)
}
\arguments{
  \item{x}{Gene expression data object. The results of vm.analysis of vm.analysis.paired function}
  \item{P}{genes with a p-value less than P will be red in the plot}
}
\details{
}
\value{
Plot of mean log-ratio versus mean log-intensity with points with a p-value less than P in red
}
\references{}
\author{Paul Delmar}
\note{}


\seealso{\code{\link{vm.analysis}},\code{\link{vm.analysis.paired}},
        \code{\link{plotvm}},\code{\link{qqplot.vm}},\code{\link{boxplotvm}}}
 }
\examples{
}
\keyword{htest}
