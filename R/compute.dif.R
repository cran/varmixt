"compute.dif" <-
function(data)
{
    mean.cond1<-apply(cond1.mat(data),1,FUN=function(x)mean(x[is.finite(x)]))
    mean.cond2<-apply(cond2.mat(data),1,FUN=function(x)mean(x[is.finite(x)]))

    var.cond1<-apply(cond1.mat(data),1,FUN=function(x)var(x[is.finite(x)]))
    var.cond2<-apply(cond2.mat(data),1,FUN=function(x)var(x[is.finite(x)]))

    N<-length(mean.cond1)
    
    RSScond1<-apply(cond1.mat(data)-mean.cond1,1,FUN=function(x)sum(x[is.finite(x)]^2))
    RSScond2<-apply(cond2.mat(data)-mean.cond2,1,FUN=function(x)sum(x[is.finite(x)]^2))

    ncond1<-apply(cond1.mat(data),1,FUN=function(x)sum(is.finite(x)))
    ncond2<-apply(cond2.mat(data),1,FUN=function(x)sum(is.finite(x)))

    RSS<-RSScond1+RSScond2
    deltag<-mean.cond2-mean.cond1
    meanint<-0.5*(mean.cond2+mean.cond1)
    df<-ncond1+ncond2-2
    varg<-RSS/df
    var.to.vardelta<-(1/ncond1+1/ncond2)
    vardeltag<-var.to.vardelta*varg
    sigmadeltag<-sqrt(vardeltag)
    test.stat.gene<-deltag/sqrt(vardeltag)
    pval.gene<-2*pmin(pt(test.stat.gene,df=df),1-pt(test.stat.gene,df=df))
    cor.pval.gene<-pmin(pval.gene*N,rep(1,N))
   
    var.anova<-mean(varg)
    vardelta.anova<-var.to.vardelta*var.anova
    sigmadeltaanova<-sqrt(vardelta.anova)
    test.stat.anova<-deltag/sigmadeltaanova
    pval.anova<-2*pmin(pnorm(test.stat.anova),1-pnorm(test.stat.anova))
    cor.pval.anova<-pmin(pval.anova*N,rep(1,N))
      
    rank.anova<-rank(test.stat.anova)
    rank.gene<-rank(test.stat.gene)
    sigmadeltaanova<-rep(sigmadeltaanova,length.out=length(test.stat.anova))
    stat1<-as.data.frame(matrix(c(deltag,sigmadeltag,meanint,varg,test.stat.gene,pval.gene,test.stat.anova,sigmadeltaanova,
                                  pval.anova,rank.anova,rank.gene,cor.pval.gene,cor.pval.anova,var.cond1,var.cond2),ncol=15))
    data$stat1<-stat1 
    names(data$stat1)<-c("deltag","sigmadeltag","meanint","varg","test.stat.gene","pval.gene","test.stat.anova",
                         "sigmadeltaanova","pval.anova","rank.anova","rank.gene","corrected.pval.gene","corrected.pval.anova",
                         "var.cond1","var.cond2")
    data$stat1.call<-match.call()
    data$df<-df
    data$var.to.vardelta<-var.to.vardelta
    residual<-cbind(cond1.mat(data)-mean.cond1,cond2.mat(data)-mean.cond2)
    data$residual<-residual
    data
}

