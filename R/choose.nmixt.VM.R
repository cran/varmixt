"choose.nmixt.VM" <-
function(data,crit=c("AIC","BIC")[2],stop.crit=1.e-6,display=TRUE,criterion=c("likelihood","parameter")[1])
{

  res<-choose.nmixt(VAR=var.gene.vect(data),df=df.vect(data),crit=crit,stop.crit=stop.crit,display=display,
                   criterion=criterion)

   data$choose.nmixt<-res$res.df.tot
   list(nmixt=res$nmixt,params=res$params,data=data)
}

