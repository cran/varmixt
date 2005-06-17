"normalize.data" <-
function(data,center=TRUE,loess.cor=FALSE)
{
    cond1<-matrix(ncol=ncol(raw.cond1.mat(data)),nrow=nrow(raw.cond1.mat(data)))
    cond2<-matrix(ncol=ncol(raw.cond2.mat(data)),nrow=nrow(raw.cond2.mat(data)))
    
    if(loess.cor==TRUE)
    {
        if(all(dim(cond1)==dim(cond2)))
        {
            for(i in 1:ncol(raw.cond1.mat(data)))
            {
                ratio<-0.5*(raw.cond2.mat(data)[,i]-raw.cond1.mat(data)[,i])
                meanint<-0.5*(raw.cond2.mat(data)[,i]+raw.cond1.mat(data)[,i])
                index<-which(is.finite(ratio) & is.finite(meanint))
                fit1<-loess(ratio[index]~meanint[index])$fitted
                ratio2<-ratio[index]-fit1
                cond1[index,i]<-meanint[index]-ratio2
                cond2[index,i]<-meanint[index]+ratio2            
            }
        }
        cond11<-scale(cond1,center=center,scale=FALSE)
        cond21<-scale(cond2,center=center,scale=FALSE)    
    } 
    else
    {
        cond11<-scale(raw.cond1.mat(data),center=center,scale=FALSE)
        cond21<-scale(raw.cond2.mat(data),center=center,scale=FALSE)        
    }
    data$cond1<-cond11
    data$cond2<-cond21
    data
}

