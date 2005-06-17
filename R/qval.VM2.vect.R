"qval.VM2.vect" <-
function(data,lambda=seq(0,0.95,0.05), pi0.meth="smoother", fdr.level=NULL, robust=FALSE)
{
 qvalue(pval.VM2.vect(data),lambda=lambda, pi0.meth=pi0.meth, fdr.level=fdr.level, robust=robust)$qvalues
}
