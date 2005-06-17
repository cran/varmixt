"compute.param.VM" <-
function(data,nmixt,sd.init=NULL,pi.init=NULL,criterion=c("likelihood","parameter")[1],
                        stop.crit=1.e-8)
{

 res<-compute.param(VAR=var.gene.vect(data),df=df.vect(data),nmixt=nmixt,sd.init=sd.init,pi.init=pi.init,
              criterion=criterion, stop.crit=stop.crit)

 res
}

