"sd.param" <-
function(data)
{
 res<-sqrt(diag(var.estim.mat(data)))
 N<-length(res)
 i1<-(N-1)/2
 pi.mat<-var.estim.mat(data)[1:i1,1:i1]
 sd.pi.last<-sqrt(sum(pi.mat))
 res[(i1+1):length(res)]<-res[(i1+1):length(res)]/2
 res.pi<-c(res[1:i1],sd.pi.last)
 res.var<-res[(i1+1):length(res)]/2
 res2<-c(res.pi,res.var)
 params<-parameters.data.frame(data)
 res3<-c(0,res2)
 res<-cbind(params,res3)
 names(res)<-c("parameter","value","sd")
 res
}
