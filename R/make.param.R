"make.param" <-
function(VAR,df,sd.init,pi.init,nmixt)
{
 st.em<-proc.time()
 niter<-niter.max
 N<-length(VAR)
 vec<-VAR*df
 c<-df/2
 var.init<-sd.init^2 
 
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
    ppost<-matrix(ncol=nmixt,nrow=N)
    gamma.dist<-matrix(ncol=nmixt,nrow=N)
    deno<-rep(0,length(vec))
    b.init<-2*var.init
    pi<-pi.init
    b<-b.init
    
    for(j in 1:nmixt)
    {  
        gamma.dist[,j]<-dgamma(vec,scale=b[j],shape=c)
    }
       
        
     deno<-as.vector(gamma.dist%*%pi)
     deno[deno==0]<-min(deno[deno>0])
        
     log.lik.cur<-sum(log(deno))
     if(is.na(log.lik.cur))
     {
         BIC<-1.e9
         stop("Cannot fit the variance model. There might be missing values")
     }
     ppost<-(gamma.dist/deno)%*%diag(pi)
  }
param.data<-list(pi=pi,b=b,ppost=ppost,c=c,deno=deno)
}

