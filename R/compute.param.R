"compute.param" <-
function(VAR,df,nmixt,sd.init=NULL,pi.init=NULL,criterion=c("likelihood","parameter")[1],
                        stop.crit=stop.crit.2)
{
 if(is.null(sd.init) | is.null(pi.init)) res<-compute.BIC.stoch(VAR=VAR,df=df,
                                                                nmixt=nmixt, stop.crit=stop.crit,criterion=criterion,
                                                                PPOST=TRUE,seed=NULL)
 else res<-compute.BIC.init(VAR=VAR,df=df,nmixt=nmixt,sd.init=sd.init,pi.init=pi.init, 
                            stop.crit=stop.crit, criterion=criterion,PPOST=TRUE, display = TRUE, VAR.OUT = FALSE, niter.max = 50000)          
 res
}
