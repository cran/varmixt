"pval.an" <-
function(data,P=.05,corrected=FALSE,display=TRUE)
{
  if(corrected)
  {
   pval.vm2<-pval.VM2.corrected.vect(data)
   pval.vm<-pval.VM.corrected.vect(data)
   pval.anova<-pval.anova.corrected.vect(data)
   pval.gene<-pval.gene.corrected.vect(data)
  }
  else
  {
   pval.vm2<-pval.VM2.vect(data)
   pval.vm<-pval.VM.vect(data)
   pval.anova<-pval.anova.vect(data)
   pval.gene<-pval.gene.vect(data)
  }
  pval.limit<-P
  n.vm2<-sum(pval.vm2<=pval.limit)
  n.vm<-sum(pval.vm<=pval.limit)
  n.anova<-sum(pval.anova<=pval.limit)
  n.gene<-sum(pval.gene<=pval.limit)
   if(display)
  {
    cat("           VM2 : n=",n.vm2,"\n")
    cat("           VM :  n=",n.vm,"\n")
    cat(" Gene-specific : n=",n.gene,"\n")
    cat("Homoescedastic : n=",n.anova,"\n")
  }
  invisible(data.frame(n.vm2,n.vm,n.anova,n.gene))
}
