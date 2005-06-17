"plotvm" <-
function(x,colors=TRUE)
{
        logvar<-log(var.gene.vect(x))
        meanint<-meanint.vect(x)
        group<-group.vect(x)
        if(colors==TRUE)
        {
            plot(meanint,logvar,xlab="Mean Intensity",ylab="Log(Var)",col=group)
        }
        else
        {
            plot(meanint,logvar,xlab="Mean Intensity",ylab="Log(Var)")
        }

}
