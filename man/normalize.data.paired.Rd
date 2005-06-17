\name{normalize.data.paired}
\alias{normalize.data.paired}
\title{Data normalization}
\description{
This function performs basic normalization on paired data
}
\usage{
normalize.data.paired(data,center=TRUE, loess.cor = FALSE)
}

\arguments{
  \item{data}{A gene expression data object. The result of the build.data.paired function}
  \item{center}{Logical. If True each array is centered to have a 0 mean log-ratio.}
  \item{loess.cor}{Logical. If True a lowess normalization is performed globally on each array.}
}
\details{The normalization is only centering of the gene log-ratio per array. After normalization 
all arrays have a mean log-ratio of 0.}
\value{
This function add the following elements to the gene expression data object :
\item{logratio}{Matrix of normalized gene expression log-ratio}
}
\references{}
\author{Paul Delmar}
\note{}

\seealso{\code{\link{build.data.paired}},\code{\link{compute.dif.paired}}}

\examples{
}
\keyword{internal}
