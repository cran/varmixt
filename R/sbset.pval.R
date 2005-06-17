"sbset.pval" <-
function(data,pval,method=c("gene","VM","VM2","anova")[2])
{
 index<-find.pval.index(data=data,pval=pval,method=method)
 keep.index(data,index)
}
