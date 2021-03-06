"vm.analysis.step.2" <-
function(data,n.mixt=NULL,filename=NULL,crit=c("AIC","BIC")[2],stop.crit.1=1.e-6,
                             criterion.1=c("likelihood","parameter")[1],criterion.2=c("likelihood","parameter")[1],
                             stop.crit.2=1.e-8,display=TRUE)
{
    if(is.null(n.mixt))
    {
        nmixt.res<-choose.nmixt.VM(data,crit=crit,stop.crit=stop.crit.1,criterion=criterion.1,display=display)
        dat1.nmixt<-nmixt.res$nmixt
        dat14<-nmixt.res$data
        params<-compute.param.VM(data=dat14,nmixt=dat1.nmixt,pi.init=nmixt.res$params$pi,
                              sd.init=sqrt(nmixt.res$params$vars),
                               criterion=criterion.2,stop.crit=stop.crit.2)
          if(dat1.nmixt>1)
        {
            res<-varmixt.an(dat14,dat1.nmixt,pi=params$pi,b=params$b,
                                   ppost=params$ppost,c=params$c,deno=params$deno)

        }
        else
        {
            res<-dat14
        }
    }
    else
    {
       res<-varmixt.an(data,n.mixt,criterion=criterion.2,stop.crit=stop.crit.2)
    }
    if(!is.null(filename))
    {
         export.res(res,paste(filename,".txt",sep=""))    
    }
    class(res)<-"varmixt.res"
    res
}
