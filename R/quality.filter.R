"quality.filter" <-
function(data,badtol)
{
  temp<-badqual.mat(data)
  temp2<-apply(temp,1,sum)
  res<-list()
  for(i in 1:length(data))
  {
      if(is.vector(data[[i]]))
      {
        res[[i]]<-data[[i]][temp2<=badtol]
      }
      else
      {
           res[[i]]<-data[[i]][temp2<=badtol,]      
      }
  }
  names(res)<-names(data)
  res$global.qual<-temp2[temp2<=badtol]
  res
}

