"comp.bic" <-
function(VAR,df,stop.crit=1.e-6,n.max=20,display=FALSE)
{
    for (i in 1:n.max)
    {
        rest<-compute.BIC(VAR=VAR,df=df,nmixt=i,stop.crit=stop.crit,display=display)
        res<-data.frame(BIC=rest$BIC,AIC=rest$AIC,loglike=rest$loglike,N=rest$N,
                        nmixt=rest$nmixt,nparam=rest$nparam)
        if(i==1) res.tot<-res
        if(i>1) res.tot<-rbind(res.tot,res)
    }
    end.time<-proc.time()
    list(res=res.tot,call=match.call())
}
