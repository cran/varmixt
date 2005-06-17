"compute.BIC" <-
function(VAR,df,nmixt,stop.crit=1.e-08,display=TRUE,criterion=c("likelihood","parameter")[2],
                      PPOST=FALSE,VAR.OUT=FALSE)
{
 st.em<-proc.time()
 niter<-50000
 N<-length(VAR)
 SSs<-sort(VAR)

 var.init<-vector(mode="numeric",length=nmixt)
 c<-df/2
  
 if(nmixt==1)
 {
    var.init<-mean(VAR)
    b.init<-2*var.init
    niter<-1
    pi.init<-1
    res.final<-compute.BIC.init(VAR=VAR,df=df,sd.init=sqrt(var.init),pi.init=pi.init,nmixt=nmixt,
                               stop.crit=stop.crit,display=FALSE,criterion=criterion)
 }
 else
 {
    var.init<-tapply(SSs,sort(rep(1:nmixt,length.out=N)),mean)
    pi.init<-rep(1/nmixt,nmixt)
    res.final<-compute.BIC.init(VAR=VAR,df=df,sd.init=sqrt(var.init),pi.init=pi.init,nmixt=nmixt,
                                stop.crit=stop.crit,display=FALSE,PPOST=PPOST,niter.max=niter,VAR.OUT=VAR.OUT,
                                criterion=criterion)
 }
 et.em<-proc.time()
 delta.t<-et.em-st.em
 if(display)
 {
    attach(res.final)
        cat("N mixt= ",nmixt,". EM algo, number of iterations=",niter.f,". Elapsed time= ",round(delta.t[3]/60,2)," min","\n",sep="")
        cat("pi=",pi,"\n")
        cat("sd=",sqrt(vars),"\n")
        cat("log-likelihood=",loglike,"\n")
        cat("BIC=",BIC,"  AIC=",AIC,"\n \n")
    detach(res.final)
 }
 invisible(res.final)
}
