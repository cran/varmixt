"compute.BIC.init" <-
function(VAR,df,sd.init,pi.init,nmixt,stop.crit=stop.crit.2,display=TRUE,PPOST=FALSE,
                           niter.max=50000,VAR.OUT=FALSE,criterion=criterion)
{
 
 st.em<-proc.time()
 niter<-niter.max
 N<-length(VAR)
 vec<-VAR*df
 c<-df/2
 var.init<-sd.init^2 

# If only one component in the mixture no need for EM algo !
 if(nmixt==1)
 {
    ppost<-0
    deno<-0
    b.init<-2*var.init
    loglike<-sum(log(dgamma(vec,scale=b.init,shape=c)))
    log.lik.cur<-loglike
    AIC<- -2*loglike+2*(2-1)
    BIC<- -2*loglike+(2-1)*log(N) 
    niter.f<-1
    b<-b.init
    vars<-b/2
    pi<-1
 }
 else
 {
# Declare variable for EM algo
    ppost<-matrix(ncol=nmixt,nrow=N)
    gamma.dist<-matrix(ncol=nmixt,nrow=N)
    deno<-rep(0,length(vec))
    b.init<-2*var.init
    pi<-pi.init
    b<-b.init
 dg <- function(sc,x,shape){
 dgamma(x=x,scale=sc,shape=shape)
 }   
 # Start EM algorithm
    for(i in 1:niter)
    {
        if(i>1)
        {
            params.old<-params.cur
            log.lik.old<-log.lik.cur
        }
       bmat <- t(as.matrix(b))
       gamma.dist <- apply(bmat,2,dg,x=vec,shape=c)
       
        deno<-as.vector(gamma.dist%*%pi)

 # Make sure all values in the denominator are >0 
        deno[deno==0]<-min(deno[deno>0])
        
        log.lik.cur<-sum(log(deno))
        if(is.na(log.lik.cur))
        {
            BIC<-1.e9
            stop("Cannot fit the variance model. There might be missing values")
        }
        ppost<-(gamma.dist/deno)%*%diag(pi)
        pi<-apply(ppost,2,mean)
        b<-apply(ppost*(vec/c),2,sum)/(pi*N)
        params.cur<-c(pi,b)    
        if (i>1) 
        {
            
            # Stop criterion based on parameters       
            if(criterion=="parameter")  crit<-max(abs((params.cur-params.old)/params.old))
            # Stop criterion based on log-likelihood
            if(criterion=="likelihood")  crit<- abs(log.lik.cur-log.lik.old)/(abs(log.lik.old))

            if (crit<stop.crit)
            {
                rm(gamma.dist)
                niter.f<-i
                vars<-b/2
                loglike<-log.lik.cur
                AIC<- -2*loglike+2*(2*nmixt-1)
                BIC<- -2*loglike+(2*nmixt-1)*log(N)    
                break
            } 
            if(i==niter)
            {
                rm(gamma.dist)
                niter.f<-i
                vars<-b/2
                loglike<-log.lik.cur
                AIC<- -2*loglike+2*(2*nmixt-1)
                BIC<- -2*loglike+(2*nmixt-1)*log(N)
                warning("EM algorithm did not converge")
                break
            }
        }   
    }
 }
 et.em<-proc.time()
 delta.t<-et.em-st.em
if(display)
{
 cat("N mixt=",nmixt,". EM algo, number of iterations=",niter.f,". Elapsed time=",round(delta.t[3]/60,2)," minute(s).\n",sep="")
 cat("pi=",pi,"\n")
 cat("sd=",sqrt(vars),"\n")
 cat("var=",vars,"\n")  
 cat("log-likelihood=",loglike,"\n")
 cat("BIC=",BIC,"  AIC=",AIC,"\n \n")
}

res<-list(BIC=BIC,AIC=AIC,pi=pi,b=b,vars=vars,c=c,loglike=loglike,
           N=N,nmixt=nmixt,nparam=(2*nmixt-1),niter.f=niter.f,sd=sqrt(vars))
if(PPOST)
{
 res$ppost<-ppost
 res$deno<-deno
}
if(VAR.OUT)
{
 group<-max.col(ppost)
 res$group<-group
 res$var<-VAR
}
invisible(res)

}
