"find.pval.index" <-
function(data,pval,method=c("gene","VM","VM2","anova")[2])
{
 res<-which(get(paste("pval.",method,".vect",sep=""))(data)<=pval)
 res
}

