"varmixt.an" <-
function(data,nmixt,pi=NULL,b=NULL,ppost=NULL,c=NULL,deno=NULL,stop.crit.2,criterion)
{
 N<-n.genes(data)    
 df<-df.vect(data)
 SS<-var.gene.vect(data)
 vec<-SS*df

 if(is.null(pi) | is.null(b) | is.null(ppost) | is.null(c))
 {
     est.param<-compute.param.VM(data=data,nmixt=nmixt,criterion = criterion,
                                 sd.init = NULL, pi.init = NULL, stop.crit = stop.crit.2)
     b<-est.param$b
     ppost<-est.param$ppost
     pi<-est.param$pi
     c<-est.param$c
     deno<-est.param$deno
 } 
 ################################################################################################ 
 # sigma (sigma2) is the vector of standard deviation (variance) of each variance group
 ################################################################################################
 sigma2<-b/2
 sigma<-sqrt(sigma2)

 ################################################################################################  
 # sigma2bis is the variance estimate of each gene as a weighted means of each of the group 
 # variance. The weights are the posterior proba.
 ################################################################################################
 sigma2bis<-ppost%*%sigma2
 sigmabis<-sqrt(sigma2bis)
 
 ################################################################################################
 # Compute the variance of the proba and variance estimates
 ################################################################################################
 var.estim<-varmat(vect=vec,nmixt=nmixt,pi=pi,b=b,c=c)
 
 ################################################################################################
 # P3[k,j] is the probability that a gene g belongs to group k
 #  given that it was assigned to group j
 ###############################################################################################
 c.unique<-unique(c)
 P3<-list()    
 for(i in 1:length(c.unique))
 {
     P3[[i]]<-compute.P3(b,pi,c.unique[i])
 }
 
 groups<-max.col(ppost)
 ################################################################################################
 # sigmag is the vector of standard deviation of each gene. The standard deviation of a gene
 # is the standard deviation of the variance component the gene is assigned to.
 ################################################################################################
 sigmag<-sigma[groups]

 var.to.vardelta<-var.to.vardelta.vect(data)
 
 sigmadelta<-sqrt(var.to.vardelta)%*%t(sigma)
 
 sigmadeltag<-sigmag*sqrt(var.to.vardelta)
 sigmadeltagbis<-sigmabis*sqrt(var.to.vardelta)
 
 deltag<-logratio.vect(data)
 teststat<-deltag/sigmadeltag
 teststatVM<-deltag/sigmadeltagbis
 ################################################################################################
 # Compute p-value in the after assigning a gene to a group
 ################################################################################################
 pval0<-rep(0,length(teststat)) 
 for(j in 1:length(c.unique))
 {
    index<-which(c==c.unique[j])
    for(i in 1:nmixt)
    {
        pval0[index]<-pval0[index]+pnorm(abs(teststat[index]),mean=0,sd=(sigma[i]/sigmag[index]))*P3[[j]][i,groups[index]] 
    }
 }
 pval<-pmax(2*(1-pval0),rep(0,N))
 rm(pval0)
 corpval<-pmin(pval*N,rep(1,N))

 ################################################################################################
 # Compute p-value without assigning a gene to a group
 ################################################################################################
 pval.vm<-rep(0,length(deltag)) 
 for(i in 1:nmixt)
 {
    pval.vm<-pval.vm+pnorm(abs(teststatVM),mean=0,sd=sigma[i]/sigmabis)*ppost[,i]
 }
 
 pval.vm<-pmax(2*(1-pval.vm),rep(0,N))
 corpval.vm<-pmin(pval.vm*N,rep(1,N))

 rankvm2<-rank(teststat)
 rankvm<-rank(teststatVM)
 stat2<-data.frame(deltag,sigmadeltag,sigmag,teststat,groups,pval,corpval,teststatVM,sigmadeltagbis,
                   pval.vm,corpval.vm,rankvm2,rankvm)
 names(stat2)<-c("deltag","sigmadeltagVM2","sigmagVM2","test.stat.VM2","group","pval.VM2","corrected.pval.VM2",
                 "test.stat.VM","sigmadeltagVM","pval.VM","corrected.pval.VM","rank.VM2","rank.VM")
 
 rm(deltag,sigmadeltag,teststat,groups,pval,corpval,teststatVM,sigmadeltagbis,
                   rankvm2,rankvm,pval.vm)
 
 parameters.name<-c("nmixt",rep("pi",length(pi)),rep("sigma2",length(sigma)))
 parameters.value<-c(nmixt,pi,sigma^2)
 parameters.data<-data.frame(param=parameters.name,value=parameters.value)

 data$stat2<-stat2
 rm(stat2)
 data$stat2.call<-match.call()
 data$param<-parameters.data
 if(exists("deno")){data$vardist<-deno}
 if(exists("var.estim")){data$var.estim<-var.estim}
 if(exists("sigmadelta")){data$sigmadelta<-sigmadelta}
 if(exists("P3")){data$P3<-P3}
 if(exists("ppost")){data$ppost<-ppost}
 data
}
