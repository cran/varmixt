\name{export.res}
\alias{export.res}
\title{Format and export analysis results}
\description{
This function builds a single data.frame with all the analysis results,
computes the rank of genes based on their p-value and computes a ratio
from the log-ratio.
}
\usage{
export.res(data,filename=NULL,header=TRUE,comment=NULL,lambda=seq(0,0.95,0.05))
}
\arguments{
  \item{data}{A gene expression data object. The results of vm.analysis of vm.analysis.paired function}
  \item{filename}{the name of the output file. If NULL no file is written}
  \item{header}{a logical value indicating whether the file contains the
          names of the variables as its first line.}
  \item{comment}{}
  \item{lambda}{}
}
\details{
}
\value{
a data.frame with all the analysis results,
computes the rank of genes based on their p-value and computes a ratio
from the log-ratio.
    \item{geneid}{gene name}
    \item{ratio}{ratio}
    \item{qual}{Number of low quality observations}
    \item{pval.VM2}{p-value with the variance mixture model}
    \item{corrected.pval.VM2}{corrected p-value with the variance mixture model(Bonferroni)}
    \item{test.stat.VM2}{test statistic with the variance mixture model}
    \item{sigmadeltagVM2}{standard deviation of deltag with the variance mixture model}
    \item{group}{variance component (variance mixture model)}
    \item{rank.p.VM2}{rank according to the variance mixture model p-value}
    \item{rank.p.VM}{rank according to the variance mixture model p-value without assigning gene to variance groups}
    \item{rank.p.gene}{rank according to the gene-specific p-value}
    \item{rank.p.anova}{rank according to the homoscedastic p-value}
    \item{pval.gene}{p-value with the gene specific model}
    \item{corrected.pval.gene}{corrected p-value with gene specific model (Bonferroni)}
    \item{test.stat.gene}{test statistic with gene-specific model}
    \item{sigmadeltag}{standard deviation of deltag with the gene-specific model}
    \item{varg}{gene-specific variance of the gene}
    \item{pval.anova}{p-value with the homoscedastic model}
    \item{corrected.pval.anova}{corrected p-value with homoscedastic model (Bonferroni)}
    \item{test.stat.anova}{test statistic with the homoscesatic model}
    \item{sigmadeltaanova}{standard deviation of deltag with the homoscesatic model}
    \item{pval.VM2}{p-value with the variance mixture model without assigning genes to variance groups}
    \item{corrected.pval.VM2}{corrected p-value with the variance mixture model(Bonferroni) without assigning genes to variance groups}
    \item{test.stat.VM}{test statistic with the continuous variance mixture model}
    \item{sigmadeltagVM}{standard deviation of deltag with the continuous variance mixture model}
    \item{rank.VM2}{rank according to the variance mixture model test statistic}
    \item{rank.gene}{rank according to homoscedastic test statistic}
    \item{rank.anova}{rank according to homoscedastic test statistic}
    \item{rank.VM}{rank according to the continuous variance mixture model test statistic}
    \item{deltag}{mean log2-ratio}
    \item{meanint}{mean log2-intensity}
 }
\references{}
\author{Paul Delmar}
\note{}

\seealso{\code{\link{vm.analysis}}, \code{\link{vm.analysis.paired}}}
\examples{
}
\keyword{htest}
