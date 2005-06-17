"compute.P3" <-
function(b,pi,c)
{
################################################################################################
# P3[k,j] is the probability that a gene g belongs to group k
#  given that it was assigned to group j
###############################################################################################


 ################################################################################################
 # This section computes the range of gene specific variance
 # corresponding to each variance group.
 ################################################################################################

 nmixt<-length(b)
 prec1<-0.001
 prec2<-1.e-5
 endint<-.05
 endpoint<-endint-prec2
 x00<-seq(prec2,endpoint,by=prec2)
 x0<-seq(endint,500,by=prec1)
 x0<-c(x00,x0)

 pigam<-matrix(ncol=nmixt,nrow=length(x0))

 for(i in 1:nmixt)
 {
   pigam[,i]<-pi[i]*dgamma(x0,scale=b[i],shape=c)
 }

 pigammax<-matrix(c(x0,max.col(pigam)),ncol=2)

 x.min<-rep(1,nmixt)
 x.max<-rep(1,nmixt)
 
 for (i in 1:nmixt) {
        if (length(pigammax[pigammax[, 2] == i, 1]) > 0) {
            x.min[i] <- min(pigammax[pigammax[, 2] == i, 1])
            x.max[i] <- max(pigammax[pigammax[, 2] == i, 1])
            
               x01 <- seq(max(c((x.min[i] - prec1), prec2)), (x.min[i] + prec1), prec2)
               x02 <- seq(max(c((x.max[i] - prec1), prec2)), (x.max[i] + prec1), prec2)
             
             if (b[i] == min(b))        x03 <- x02
             else if (b[i] == min(b))   x03 <- x01
             else                       x03 <- c(x01, x02) 
             pigam2 <- matrix(ncol = nmixt, nrow = length(x03))
             for (j in 1:nmixt) {
                  pigam2[, j] <- pi[j] * dgamma(x03, scale = b[j], shape = c)
                }
              pigammax2 <- matrix(c(x03, apply(pigam2, 1, FUN = function(x) match(max(x, 
                  na.rm = TRUE), x))), ncol = 2)
             if (b[i] == min(b)) x.min[i]=1e-08
             else                x.min[i] <- min(pigammax2[pigammax2[, 2] == i,1])
          }
    }
    
    x.max<-c(x.min[2:nmixt],500)
    
 rm(pigam,pigammax,pigammax2,pigam2)
 ################################################################################################
 # P1[j,k] is the probability of assigning gene g to group j when gene g belongs
 # to group k.
 ################################################################################################
 P1<-matrix(ncol=nmixt,nrow=nmixt)

 ################################################################################################
 #P2[j] is the unconditionnal probability that a gene g is assigned to group j 
 ################################################################################################
 P2<-vector(length=nmixt)

 ################################################################################################
 # P3[k,j] is the probability that a gene g belongs to group k
 #  given that it was assigned to group j
 ################################################################################################
 P3<-matrix(ncol=nmixt,nrow=nmixt)

 for (j in 1:nmixt)
 {
    if(x.max[j]>(x.min[j]+0.9*prec2))
    {
        for(k in 1:nmixt)
        {
            term1<-pgamma(x.max[j], scale = b[k], shape = c)
            term2<-pgamma(x.min[j], scale = b[k], shape = c)
            if (b[j] == min(b)) 
                { term2==0 
                }
            if (b[j] == max(b)) 
                { term1==1 
                }
            P1[j, k] <- term1 - term2

 }
 }
 }
 
 P2=P1%*%pi
  
 for(j in 1:nmixt)
 {
 Ptmp=t(P1)*pi
         if((P2[j]>0) & is.na(P2[j])==FALSE)
         {
         P3[,j]<-Ptmp[,j]/P2[j]
         }
 }
 P3
}
