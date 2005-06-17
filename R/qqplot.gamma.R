"qqplot.gamma" <-
function(vars,df,PLOT=TRUE,...)
{
    N<-length(vars)
    n<-N-1     
    z<-df*vars
    z.sort<-sort(z)
    z.sort<-z.sort[1:(N-1)]
    cc<-df/2
    quant.vect<-(1:(N-1))*(1/N)
    quant.gamma<-qgamma(quant.vect,scale=1,shape=cc)
    
    x<-quant.gamma
    y<-z.sort
    b<-(sum(x*y)-n*mean(x)*mean(y))/(sum(x^2)-n*mean(x)^2)
    a<-mean(y)-b*mean(x)
    y.pred<-b*x+a
    residual<-y-y.pred
    if(length(residual)>2) var.res<-var(residual)
    else var.res<-0
    p<-sum(x*y)/sum(x^2)
    y.pred2<-p*x
    residual2<-y-y.pred2
    if(length(residual2)>2) var.res2<-var(residual2)
    else var.res2<-0
    # BIC
    loglike<-sum(log(dnorm(y,y.pred2,sqrt(var.res2))))
    BIC <- -loglike+2*log(N)
    # R^2
    if(length(x)>2 & length(y)>2) Rsquare<-cor(x,y)^2
    else Rsquare<-1
    
    if(PLOT)
    {
        plot(quant.gamma,y,...)
        lines(x,y.pred,lwd=2,col="red")
        lines(x,y.pred2,lwd=2,col="blue")
        cat("N=",N,"\n")
        cat("y = a + bx + e\n")
        cat("    a=",a,"; b=",b,"; VAR(e)=",var.res,"\n")
        cat("y = px + e \n")
        cat("    p=",p,"; VAR(e)=",var.res2,"\n")
        cat("loglikelihood=",loglike,"\n")
        cat("BIC=",BIC,"\n") 
        cat("R^2=",Rsquare,"\n\n")
    }
    var.res
    var.res2
  invisible(1-Rsquare)
}
