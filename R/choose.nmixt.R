"choose.nmixt" <-
function(VAR,df,crit=c("AIC","BIC")[2],stop.crit=stop.crit.2,display=TRUE,
                       criterion=c("likelihood","parameter")[1])
{
   res<-list()
   res.crit<-vector()
   nmixt<-1
   
   for (i in 1:20)
   {
        res[[i]]<-compute.BIC(VAR=VAR,df=df,nmixt=i,stop.crit=stop.crit,
                              display=display,criterion=criterion,PPOST = FALSE, VAR.OUT = FALSE)
        res.crit[i]<-unlist(res[[i]][crit])

        res.df<-data.frame(BIC=res[[i]]$BIC,AIC=res[[i]]$AIC,loglike=res[[i]]$loglike,N=res[[i]]$N,
                        nmixt=res[[i]]$nmixt,nparam=res[[i]]$nparam)
        if(i==1) res.df.tot<-res.df
        if(i>1) res.df.tot<-rbind(res.df.tot,res.df)

        if(i>2)
        {
            if(res.crit[i]>res.crit[i-1])
            {
                nmixt<-i-1
                break
            }
        }
   }
    cat("nmixt=",nmixt,"\n")
   list(nmixt=nmixt,params=res[[i-1]],res.df.tot=res.df.tot)

}
