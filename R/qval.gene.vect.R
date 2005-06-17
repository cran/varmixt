"qval.gene.vect" <-
function(data,lambda=seq(0,0.95,0.05), pi0.meth="smoother", fdr.level=NULL, robust=FALSE)
{
 qvalue(pval.gene.vect(data),lambda=lambda, pi0.meth=pi0.meth, fdr.level=fdr.level, robust=robust)$qvalues
}
