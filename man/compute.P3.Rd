\name{compute.P3}
\alias{compute.P3}
\title{Internal Function for computing P-value in variance mixture model}
\description{
 P3[k,j] is the probability that a gene g belongs to group k        
 given that it was assigned to group j
 }
\usage{
compute.P3(b, pi, c)
}
\arguments{
  \item{b}{vector of parameter for the gamma distribution}
  \item{pi}{vector of prior probability of belonging to each group}
  \item{c}{Scalar : parameter of the gamma distribution}
}
\details{
}
\value{
A matrix P3 such that, P3[k,j] is the probability that a gene g belongs to group k        
 given that it was assigned to group j }
\references{}
\author{Paul Delmar}
\note{}

\seealso{\code{\link{varmixt.an}}}
\examples{}
\keyword{internal}
