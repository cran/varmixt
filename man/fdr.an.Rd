\name{fdr.an}
\alias{fdr.an}

\title{FDR analysis of variance mixture analysis results}
\description{This functions computes the number of regulated genes
at given level of FDR}
\usage{
fdr.an(data,Q=0.05,display=TRUE,lambda=seq(0,0.95,0.05))
}
\arguments{
  \item{data}{A gene gene expression data object. The results of vm.analysis or vm.analysis.paired function}
  \item{Q}{the fdr level}
  \item{display}{if TRUE prints the number of regulated genes and corresponding p-values for the different methods on the screen}
  \item{lambda}{Argument to the q-value function.}
}
\details{If lambda=0, then the FDR is computed (Benjamini Hochberg). If lambda is set to its default, the
the pFDR according to Storey is computed.
}
\value{
  a data frame with the number of genes regulated with
  the different analysis methods and the corresponding p-value}
\references{Benjamini, Y. and Y. Hochberg (1995). Controlling the false discovery rate: a practical and powerful approach to multiple testing.
J. R. Statist. Soc. B 57(1), 289-300
Storey JD. (2002) A direct approach to false discovery rates. Journal of the Royal Statistical Society, Series B, 64: 479-498}
\author{Paul Delmar}
\note{}
\seealso{\code{\link{vm.analysis}},
\code{\link{vm.analysis.paired}}, \code{\link{pval.an}}
}
\examples{}
\keyword{htest}
