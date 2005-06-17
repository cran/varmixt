"plotrm" <-
function(x,P=0.05)
{
 log2ratio<-logratio.vect(x)
 pvals<-pval.VM2.vect(x)
 toto<-rep("black",length(pvals))
 toto[pvals<=P]<-"red"
 meanint<-meanint.vect(x)
 plot(meanint,log2ratio,xlab="Mean Intensity",ylab="Log2 Ratio",col=toto)
}
