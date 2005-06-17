"stat1.data.frame" <-
function(data)
{
  if(!is.null(data$stat1))
  {
     res<-as.data.frame(data$stat1)
  }
  else
  {
     res<-NULL
  }
  res
}

