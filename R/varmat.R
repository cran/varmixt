"varmat" <-
function(vect,nmixt,pi,b,c)
{
    varcovar<-matrix(0,ncol=(2*nmixt-1),nrow=(2*nmixt-1))
    N<-length(vect)
    for(i in 1:N)
    {
            varcovar<-varcovar+dif.f(x=vect[i],nmixt=nmixt,pi=pi,b=b,c=c[i]);
    }
    varcovar<-ginv(varcovar)
    varcovar
}
