"comp.bic.stoch" <-
function(VAR,df,stop.crit=1.e-6,n.max=20)
{
    start.time<-proc.time()
    for (i in 1:n.max)
    {
        rest<-compute.BIC.stoch(VAR=VAR,df=df,nmixt=i,stop.crit=stop.crit)
        res<-data.frame(BIC=rest$BIC,AIC=rest$AIC,loglike=rest$loglike,N=rest$N,
                        nmixt=rest$nmixt,nparam=rest$nparam)
        if(i==1) res.tot<-res
        if(i>1) res.tot<-rbind(res.tot,res)
    }
    end.time<-proc.time()
    delta.time<-(end.time-start.time)[3]
    cat("TOTAL ELAPSED TIME=",delta.time/60,"minutes \n")
    list(res=res.tot,call=match.call())
}

