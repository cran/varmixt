"qqplot.vm" <-
function(data,...)
{
 groups.vect<-group.vect(data)
 ngroups<-length(unique(groups.vect))
 n.rows<-2
 n.cols<-min(ceiling(ngroups/n.rows),3)
 residu<-residual.mat(data)
 residu.a<-as.vector(as.matrix(residu))
 residu.a<-residu.a[is.finite(residu.a)]
 if ((n.rows*n.cols-length(unique(groups.vect)))==1){
 par(mfrow=c(n.rows,n.cols))
 }
 if ((n.rows*n.cols-length(unique(groups.vect)))==0){
 par(mfrow=c(n.rows,n.cols+1))
 }
 qqnorm(residu.a,main="all")
 k<-0
 for(i in sort(unique(groups.vect)))
 {
  residu2<-residu[groups.vect==i,]
  residu3<-as.vector(as.matrix(residu2))
  residu3<-residu3[is.finite(residu3)]
  qqnorm(residu3,main=as.character(i))
  k<-k+1
  if(k%%(n.rows*n.cols)==0 & k<length(unique(groups.vect))) x11()
 }
}
