"vm.analysis.paired" <-
function(geneId,ratio,meanint=NULL,filename=NULL,gene.anot=NULL,badqual=NULL,qualtol=NULL,n.mixt=NULL,
                             center=TRUE,loess.cor=FALSE,min.rep=2,penalty=c("AIC","BIC")[2],display=TRUE,
                             criterion.1=c("likelihood","parameter")[1], stop.crit.1=1.e-6,
                             criterion.2=c("likelihood","parameter")[1], stop.crit.2=1.e-8)
{
    start.time<-proc.time()


    dat14<-vm.analysis.paired.step.1(geneId=geneId,ratio=ratio,meanint=meanint,gene.anot=gene.anot,
                                     badqual=badqual,qualtol=qualtol,center=center,loess.cor=loess.cor,
                                     min.rep=min.rep)

    res<-vm.analysis.step.2(data=dat14,n.mixt=n.mixt,filename=filename,crit=penalty,display=display,stop.crit.1=stop.crit.1,
                             criterion.1=criterion.1,criterion.2=criterion.2,
                             stop.crit.2=stop.crit.2)
    res$call<-match.call()
    rm(dat14)
    
    end.time<-proc.time()
    delta.time<-end.time-start.time
    cat("TOTAL ELAPSED TIME : ",round(delta.time[3]/60,3)," min","\n",sep="")
    class(res)<-"varmixt.res"
    invisible(res)
}
