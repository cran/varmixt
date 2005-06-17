"normalize.data.paired" <-
function(data,center=TRUE,loess.cor=FALSE)
{

  if(loess.cor)
  {
        if(!is.null(raw.meanint.mat(data)))
        {
            raw.meanint<-raw.meanint.mat(data)
            raw.log.ratio<-raw.log.ratio.mat(data)        
            ratio.loess<-matrix(ncol=ncol(raw.log.ratio),nrow=nrow(raw.log.ratio))
            for(i in 1:ncol(raw.log.ratio))
            {
                ratio<-raw.log.ratio[,i]
                meanint<-raw.meanint[,i]
                index<-which(is.finite(ratio) & is.finite(meanint))
                fit1<-loess(ratio[index]~meanint[index])$fitted
                ratio2<-ratio[index]-fit1
                ratio.loess[index,i]<-ratio2
             }
             if(center)
             {
                ratio.loess.m<-apply(ratio.loess,2,FUN=function(x)mean(x[is.finite(x)]))
                ratio.norm<-sweep(ratio.loess,2,ratio.loess.m)     
             }
             else
             {
                ratio.norm<-ratio.loess
             }
             rm(ratio.loess,ratio.loess.m)
             data$meanint<-raw.meanint
         }
         else
         {
            ratio.dat<-raw.log.ratio.mat(data)
            if(center)
            {
                ratio.dat.m<-apply(ratio.dat,2,FUN=function(x)mean(x[is.finite(x)]))
                ratio.norm<-sweep(ratio.dat,2,ratio.dat.m)
            }
            else
            {
                ratio.norm<-ratio.dat
            }
            rm(ratio.dat,ratio.dat.m)
            warning("Cannot perform lowess normalization because of lack of mean log-intensity data")
         }
        rm(raw.meanint,raw.log.ratio) 
   }
   else
   {
        ratio.dat<-raw.log.ratio.mat(data)
        if(center)
        {
            ratio.dat.m<-apply(ratio.dat,2,FUN=function(x)mean(x[is.finite(x)]))
            ratio.norm<-sweep(ratio.dat,2,ratio.dat.m)
        }
        else
        {
            ratio.norm<-ratio.dat        
        }
        rm(ratio.dat,ratio.dat.m)
        if(!is.null(raw.meanint.mat(data))) {data$meanint<-raw.meanint.mat(data)}
   }
   
   data$log.ratio<-ratio.norm
   rm(ratio.norm)    
   data
}

