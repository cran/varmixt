"sbset.qval" <-
function(data,qval,method=c("gene","VM","VM2","anova")[2],lambda=seq(0,0.95,0.05))
{
 index<-find.qval.index(data=data,qval=qval,method=method,lambda=lambda)
 keep.index(data,index)
}
