\name{fdr}
\alias{fdr}
\title{Computation of FDR from a vector of P-value}
\description{
 This function computes the p-value cut-off for a given FDR level.
 This program was developped by Benjamini et al.
}
\usage{
fdr(p, Q = 0.05, method = c("stepup", "adaptive", "stepdown", "dependent"), plt = F, subject = "False Discovery Rate (FDR) Controlling Screening of Multiple Tests")
}
\arguments{
  \item{p}{ }
  \item{Q}{}
  \item{method}{}
  \item{plt}{}
  \item{subject}{}
}
\details{
}
\value{
}
\references{ http://www.math.tau.ac.il/ybenja/}
\author{Yoav Benjamini et al.}
\note{}

\seealso{\code{\link{fdr.an}}}
\examples{
}
\keyword{internal}
