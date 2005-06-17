"fdr.an" <-
function(data,Q=0.05,display=TRUE,lambda=seq(0,0.95,0.05))
{
  pval.vm2<-pval.VM2.vect(data)
  rest<-qvalue(p=pval.vm2,fdr.level=Q,lambda=lambda)
  pval.limit.vm2<-max(max(rest$pvalues[rest$significant]),0)
  n.vm2<-sum(pval.vm2<=pval.limit.vm2)
  
  pval.vm<-pval.VM.vect(data)
  rest<-qvalue(pval.vm,fdr.level=Q,lambda=lambda)
  pval.limit.vm<-max(max(rest$pvalues[rest$significant]),0)
  n.vm<-sum(pval.vm<=pval.limit.vm)
  n.vm<-sum(rest$significant)
  
  pval.anova<-pval.anova.vect(data)
  rest<-qvalue(pval.anova,fdr.level=Q,lambda=lambda)
  pval.limit.anova<-max(max(rest$pvalues[rest$significant]),0)
  n.anova<-sum(pval.anova<=pval.limit.anova)

  pval.gene<-pval.gene.vect(data)
  rest<-qvalue(pval.gene,fdr.level=Q,lambda=lambda)  
  pval.limit.gene<-max(max(rest$pvalues[rest$significant]),0)
  n.gene<-sum(pval.gene<=pval.limit.gene)
  
  if(display)
  {
    cat("Total Number of gene probes=",n.genes(data),"\n",sep="")
    cat("Number of significant probes with model :\n")
    cat("                                    VM2 : n= ",n.vm2  , " (pval=", pval.limit.vm2,  ")\n", sep="")
    cat("                                     VM : n= ",n.vm   , " (pval=", pval.limit.vm,   ")\n", sep="")
    cat("                          Gene-specific : n= ",n.gene , " (pval=", pval.limit.gene, ")\n", sep="")
    cat("                          Homoscedastic : n= ",n.anova, " (pval=", pval.limit.anova,")\n", sep="")
  }
 invisible(data.frame(n.vm2=n.vm2,pval.vm2=pval.limit.vm2,n.vm=n.vm,pval.vm=pval.limit.vm,n.gene=n.gene,pval.gene=pval.limit.gene,
             n.anova=n.anova,pval.anova=pval.limit.anova))
}
