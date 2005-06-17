"plotsdt.VM" <-
function(x,pval=0.05)
{
    col.vect<-ifelse(pval.VM.vect(x)<pval,"red","black")
    plot(sd.delta.gene.vect(x),test.stat.VM.vect(x),col=col.vect,xlab="Usual SD estimate",ylab="Test statitic")
   
}
