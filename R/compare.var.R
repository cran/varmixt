"compare.var" <-
function(data)
{
 ratio.mat.treat<-logratio.mat(data$treat)
 ratio.mat.cont<-logratio.mat(data$cont)
 var.treat<-var.gene.vect(data$treat)
 var.cont<-var.gene.vect(data$cont)
 var.ratio<-rep(1,nrow(ratio.mat.cont))
 var.ratio.pval<-rep(1,nrow(ratio.mat.cont))
 for(i in 1:nrow(ratio.mat.treat))
 {
  res<-var.test(ratio.mat.treat[i,],ratio.mat.cont[i,])
  var.ratio[i]<-res$estimate
  var.ratio.pval[i]<-res$p.value
 }
 data.frame(var.treat,var.cont,var.ratio,var.ratio.pval)
}
