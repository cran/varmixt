"histresid.vm" <-
function(x,...)
{
 groups.vect<-group.vect(x)
 ngroups<-length(unique(groups.vect))
 n.rows<-2
 n.cols<-ceiling(ngroups/n.rows)
 residu<-residual.mat(x)
 par(mfrow=c(n.rows,n.cols))
 residu.a<-as.vector(as.matrix(residu))
 residu.a<-residu.a[is.finite(residu.a)]
 if ((n.rows*n.cols-length(unique(groups.vect)))==1){
 par(mfrow=c(n.rows,n.cols))
 }
 if ((n.rows*n.cols-length(unique(groups.vect)))==0){
 par(mfrow=c(n.rows,n.cols+1))
 }
 hist(residu.a,main="all",...)
 for(i in sort(unique(groups.vect)))
 {
  residu2<-residu[groups.vect==i,]
  residu3<-as.vector(as.matrix(residu2))
  residu3<-residu3[is.finite(residu3)]
  hist(residu3,main=as.character(i),...)
 }
}
