"find.qval.index" <-
function(data,qval,method=c("gene","VM","VM2","anova")[2],lambda=seq(0,0.95,0.05))
{
 res<-which(get(paste("qval.",method,".vect",sep=""))(data,lambda=lambda)<=qval)   
 res
}

