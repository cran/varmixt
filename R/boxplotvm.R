"boxplotvm" <-
function(x)
{
 meanint<-meanint.vect(x)
 group<-signif(log(sd.delta.VM2.vect(x)),3) 
 boxplot(meanint~group,xlab="Mean Intensity",ylab="Log(sd) VM model (delta_g)",varwidth=TRUE,horizontal=TRUE)
}
