\name{spleen.data.vm}
\alias{spleen.data.vm}
\docType{data}
\title{Spleen Data set}
\description{
 An example data set for paired data analysis.
 A list with the normalized Spleen data set as used in the original article.
}
\usage{data(spleen.data.vm)}
\format{
 spleen.data.vm is a list wiht 3 elements :
 \item{spleen.geneid}{A vector with gene names}
 \item{spleen.logratio}{A Matrix with 4360 rows and 6 columns with normalized log-ratio}
 \item{spleen.meanint}{A Matrix with 4360 rows and 6 columns with normalized mean log-intensity}
}

\references{P. Delmar, Robin, S., Tronik-Le Roux S. and Daudin J.-J. (2005) Mixture model on the variance for the differential analysis of gene expression data, JRSS series C, 54(1), 31:50}

\examples{
data(spleen.data.vm)
}
\keyword{datasets}
