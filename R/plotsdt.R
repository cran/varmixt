"plotsdt" <-
function(x,pval.f,test.stat.f,pval=0.05)
{
    col.vect<-ifelse(pval.f(x)<pval,"red","black")
    plot(sd.delta.gene.vect(x),test.stat.f(x),col=col.vect)
    title(xlab="Usual SD estimate",ylab="Test statitic")
}
