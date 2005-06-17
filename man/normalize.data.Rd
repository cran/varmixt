\name{normalize.data}
\alias{normalize.data}
\title{Data Normalization}
\description{
This function performs basic normalization on unpaired data.
}
\usage{
normalize.data(data, center=TRUE,loess.cor = FALSE)
}
\arguments{
  \item{data}{A gene expression data object. The result of build.data function}
  \item{center}{Logical. If True each array*condition is centered to have a 0 mean log-intensity.}
  \item{loess.cor}{Logical. If True a lowess normalization is performed globally on each array.}
}
\details{The normalization is only centering of the gene expression per array and per condition. After normalization 
all arrays  have a mean expression level of 0 in both channels.}
\value{This function add the following elements to the gene expression data object :
\item{cond1}{Matrix of normalized gene expression in condition 1}
\item{cond2}{Matrix of normalized gene expression in condition 2}
 
}
\references{}
\author{Paul Delmar}
\note{}

\seealso{\code{\link{build.data}},\code{\link{compute.dif}}}

\examples{
}
\keyword{internal}
