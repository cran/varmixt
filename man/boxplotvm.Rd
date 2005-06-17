\name{boxplotvm}
\alias{boxplotvm}


\title{ Boxplot of mean log-intensity in each variance group }
\description{
 Boxplot of mean log-intensity in each variance group 
}
\usage{
boxplotvm(x)
}

\arguments{
  \item{x}{Gene expression data object}
}
\details{
The y axis is the standard deviation of mean log-ratio or mean difference.
The y axis is the denominator of the test statistic
}
\value{
A boxplot of the distribution of mean log-intensity in each variance group}
\references{}
\author{Paul Delmar}
\note{}

\seealso{\code{\link{vm.analysis}},\code{\link{vm.analysis.paired}},
        \code{\link{plotrm}},\code{\link{qqplot.vm}},\code{\link{plotvm}}}
 }
\examples{}
\keyword{htest}
