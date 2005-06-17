"qqplot.vm.2" <-
function(data,i,...)
{
 groups.vect<-group.vect(data)
 ngroups<-length(unique(groups.vect))
 n.rows<-2
 n.cols<-ceiling(ngroups/n.rows)
 residu<-residual.mat(data)
 residu2<-residu[groups.vect==i,]
 residu3<-as.vector(as.matrix(residu2))
 residu3<-residu3[is.finite(residu3)]
 qqnorm(residu3,...)
}
