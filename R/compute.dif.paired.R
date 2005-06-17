"compute.dif.paired" <-
function(data)
{
    log.ratio.mat<-logratio.mat(data)
    mean.ratio.vect<-apply(log.ratio.mat,1,FUN=function(x)mean(x[is.finite(x)]))
    RSS.ratio.vec<-apply(log.ratio.mat-mean.ratio.vect,1,FUN=function(x)sum(x[is.finite(x)]^2))

    residual<-log.ratio.mat-mean.ratio.vect
    data$residual<-residual
    rm(residual)

    N<-length(mean.ratio.vect)
    n<-apply(log.ratio.mat,1,FUN=function(x)sum(is.finite(x)))

    deltag<-mean.ratio.vect

    rm(log.ratio.mat,mean.ratio.vect)

    df<-n-1
    varg<-RSS.ratio.vec/df
    
    rm(RSS.ratio.vec)
    
    var.to.vardelta<-1/n
    vardeltag<-var.to.vardelta*varg
    sigmadeltag<-sqrt(vardeltag)
    test.stat.gene<-deltag/sqrt(vardeltag)
    pval.gene<-2*pmin(pt(test.stat.gene,df=df),1-pt(test.stat.gene,df=df))
    cor.pval.gene<-pmin(pval.gene*N,rep(1,N))

    var.anova<-mean(varg)
    vardelta.anova<-(1/n)*var.anova
    sigmadeltaanova<-sqrt(vardelta.anova)

    test.stat.anova<-deltag/sqrt(vardelta.anova)
    pval.anova<-2*pmin(pnorm(test.stat.anova),1-pnorm(test.stat.anova))
    cor.pval.anova<-pmin(pval.anova*N,rep(1,N))

    rank.anova<-rank(test.stat.anova)
    rank.gene<-rank(test.stat.gene)
    sigmadeltaanova<-rep(sigmadeltaanova,length.out=length(test.stat.anova))
  
  
    stat1<-as.data.frame(matrix(c(deltag,sigmadeltag,varg,test.stat.gene,pval.gene,test.stat.anova,sigmadeltaanova,pval.anova
                                  ,rank.anova,rank.gene,cor.pval.gene,cor.pval.anova),ncol=12))
    names(stat1)<-c("deltag","sigmadeltag","varg","test.stat.gene","pval.gene","test.stat.anova","sigmadeltaanova",
                         "pval.anova","rank.anova","rank.gene","corrected.pval.gene","corrected.pval.anova")
    
    data$stat1<-stat1
    data$df<-df
    data$var.to.vardelta<-var.to.vardelta
        
    if(!is.null(meanint.mat(data)))
    {
        meanint<-apply(meanint.mat(data),1,FUN=function(x)mean(x[is.finite(x)]))
        data$stat1$meanint<-meanint
    }
    
     data$stat1.call<-match.call()
    data
}
