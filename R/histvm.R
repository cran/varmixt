"histvm" <-
function(x,...)
{
 group<-group.vect(x)
 sd<-signif(sigmaV2.vect(x),3)
 res<-hist(group,breaks=c(min(unique(group))-0.5,unique(group)+0.5),main=NULL)
 title(...)
 res$counts
}
