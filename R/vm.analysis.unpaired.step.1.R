"vm.analysis.unpaired.step.1" <-
function(geneId,cont,treat,gene.anot=NULL,badqual=NULL,qualtol=NULL,
                                      center=TRUE,loess.cor=FALSE,min.rep=2)
{    
    if(!is.null(badqual))
    {
        dat1<-build.data(name=geneId,cond1=cont,cond2=treat,badqual=badqual,gene.anot=gene.anot,min.rep=min.rep)
        if(!is.null(qualtol))
        {
           dat12<-quality.filter(dat1,qualtol)
        }
        else
        {
            dat12<-dat1
        }
    }
    else
    {
         dat12<-build.data(name=geneId,cond1=cont,cond2=treat,gene.anot=gene.anot,min.rep=min.rep)
    }
    dat13<-normalize.data(dat12,center=center,loess.cor=loess.cor)
    rm(dat12)
    dat14<-compute.dif(dat13)
    rm(dat13)
    invisible(dat14)
}

