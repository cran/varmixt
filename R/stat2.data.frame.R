"stat2.data.frame" <-
function(data)
{
  if(!is.null(data$stat2))
  {
     res<-as.data.frame(data$stat2)
  }
  else
  {
     res<-NULL
  }
  res
}

