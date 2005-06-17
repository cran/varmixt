"plotsdm" <-
function(x,colors=TRUE)
{
        logvar<-log(sigma.delta.gene.vect(x))
        meanint<-meanint.vect(x)
        group<-group.vect(x)
        if(colors==TRUE)
        {
            plot(meanint,logvar,xlab="Mean Intensity",ylab="Log(sd)",col=group)
        }
        else
        {
            plot(meanint,logvar,xlab="Mean Intensity",ylab="Log(sd)")
        }
}
