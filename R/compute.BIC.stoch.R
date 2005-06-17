"compute.BIC.stoch" <-
function(VAR,df,nmixt,stop.crit = stop.crit.2,seed = NULL,criterion = criterion,PPOST = FALSE)
{
 if(!is.null(seed))
 {
    set.seed(seed)
 }
 st.em<-proc.time()
 niter<-50000
 N<-length(VAR)
 SSs<-sort(VAR)
 var.init<-vector(mode="numeric",length=nmixt)

 if(nmixt==1)
 {
    var.init<-mean(VAR)
    b.init<-2*var.init
    niter<-1
    pi.init<-1
    res.final<-compute.BIC.init(VAR=VAR,df=df,sd.init=sqrt(var.init),pi.init=pi.init,nmixt=nmixt,
                               stop.crit=stop.crit,display=FALSE,criterion=criterion,
                               niter.max = niter, PPOST = FALSE, VAR.OUT = FALSE)
 }
 else
 {
# Initialisation 
    n.iter.start<-30   # number of random initialization
    prec.start<-1.e-4  # precision to reach in initialization step
    niter.max<-100     # maximum number of iteration of EM algo in initialization steps
    n<-floor(length(SSs)/nmixt)
    res<-list()

    #############
    #    png("init.em%d.png")
    ############## 
    
    for(k in 1:(n.iter.start+1))
    {
        comp.l<-rep(0,nmixt)
        var.init<-vector()
        pi.init<-vector()
        cuts<-vector()
    
        if(k==1)
        {
            tmp <- cbind(SSs,sort(rep(1:nmixt,length.out=length(SSs))))
            var.init <- as.vector(aggregate(tmp[,1],list(index=tmp[,2]),mean)[,2])
            pi.init<-rep(1/nmixt,nmixt)
        }
        else
        {        
            while(min(comp.l)<=2)
            {
                cuts<-sort(sample(N,(nmixt-1)))
                cuts<-c(1,cuts,N)
                comp.l <- diff(cuts,1)+1
            }
             tmp0 <- diff(cuts)
             tmp0[1] <- tmp0[1]+1
             tmp1 <- cbind(SSs,sort(rep(2:length(cuts),tmp0)))
             var.init <- as.vector(aggregate(tmp1[,1],list(index=tmp1[,2]),mean)[,2]) 
             pi.init <- as.vector(tmp0/N) 
        }

        res[[k]]<-compute.BIC.init(VAR=VAR,df=df,sd.init=sqrt(var.init),pi.init=pi.init,nmixt=nmixt,
                                   stop.crit=prec.start,display=FALSE,niter.max=niter.max,criterion=criterion,
                                   PPOST = FALSE, VAR.OUT = FALSE)
       
    }
   
    res.loglik<-sapply(res,FUN=function(x)x$loglike)
    max.ind<-which.max(res.loglik)
    
    var.init<-res[[max.ind]]$vars
    pi.init<-res[[max.ind]]$pi
    b.init<-2*var.init

    et.em.2<-proc.time()
    delta.t.init<-(et.em.2-st.em)[3]/60
    cat("End of initialisation step in",delta.t.init,"min. ",n.iter.start,"random starting points.\n")
    cat("max.ind=",max.ind,". Log-likelihood=",res.loglik[max.ind],"\n \n")

# End of initialisation 
 
    res.final<-compute.BIC.init(VAR=VAR,df=df,sd.init=sqrt(var.init),pi.init=pi.init,nmixt=nmixt,
                                stop.crit=stop.crit,display=FALSE,VAR.OUT=FALSE,criterion=criterion,PPOST=PPOST)
                           
}
et.em<-proc.time()
delta.t<-et.em-st.em
attach(res.final)
    cat("N mixt= ",nmixt,". EM algo, number of iterations= ",niter.f,". Elapsed time= ",round(delta.t[3]/60,2)," min","\n",sep="")
    cat("pi=",pi,"\n")
    cat("sd=",sqrt(vars),"\n")
    cat("log-likelihood=",loglike,"\n")
    cat("BIC=",BIC,"  AIC=",AIC,"\n \n")
detach(res.final)
invisible(res.final)
}
