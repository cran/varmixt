"boxplotvm.g" <-
function(x)
{
 meanint<-meanint.vect(x)
 group<-signif(sd.g.VM2.vect(x),3)
 boxplot(meanint~group,xlab="Mean Intensity",ylab="Log(sd) VM model  (g)",varwidth=TRUE,horizontal=TRUE)
}
