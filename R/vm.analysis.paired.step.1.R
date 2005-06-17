"vm.analysis.paired.step.1" <-
function(geneId,ratio,meanint=NULL,gene.anot=NULL,badqual=NULL,qualtol=NULL,
                                    center=TRUE,loess.cor=FALSE,min.rep=2)
{

    dat1<-build.data.paired(name=geneId,ratio=ratio,meanint=meanint,badqual=badqual,gene.anot=gene.anot,min.rep=min.rep)
    if(!is.null(qualtol))
    {
        dat12<-quality.filter(dat1,qualtol)
    }
    else
    {
        dat12<-dat1
    }
    dat13<-normalize.data.paired(data=dat12,center=center,loess.cor=loess.cor)
    rm(dat12)
    dat14<-compute.dif.paired(dat13)
    rm(dat13)
    invisible(dat14)
}

