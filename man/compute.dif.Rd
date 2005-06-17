\name{compute.dif}
\alias{compute.dif}
\title{Function to compute simple statistics with unpaired data}
\description{
This function computes a number of statistics for each gene in the unpaired data case.
In particular, it computes the result of the homoscedastic and gene-specific variance model.
}
\usage{
compute.dif(data)
}
\arguments{
  \item{data}{A gene expression data object. The result of the normalize.data function.}
}
\details{
}
\value{
This function adds the following elements to the gene expression data object :
\item{stat1.call}{ The call to the function}
\item{df}{The number of degrees of freedom used in the computation of each gene variance}
\item{var.to.vardelta} {The factor to go from the gene variance to the variance of the mean difference}
\item{residual}{A matrix with the residual}
}
\references{}
\author{Paul Delmar}
\note{}
\seealso{\code{\link{vm.analysis}},\code{\link{normalize.data}}}

\examples{
}
\keyword{internal}
