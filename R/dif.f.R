"dif.f" <-
function(x,nmixt,pi,b,c)
{
    dfdb<-rep(0,nmixt)
    deno<-rep(0,length(x))
    for(j in 1:nmixt)
    {
        deno<-deno+pi[j]*dgamma(x,scale=b[j],shape=c)
    }
    f<-log(deno)
    for(k in 1:nmixt)
    {
        dfdb[k]<-(-pi[k]*dgamma(x,scale=b[k],shape=c)*(c/b[k]-x/(b[k]*b[k])))/deno
    }
    dfdpi<-rep(0,(nmixt-1))
    for(k in 1:(nmixt-1))
    {
        dfdpi[k]<-(dgamma(x,scale=b[k],shape=c)-dgamma(x,scale=b[nmixt],shape=c))/deno
    }
    df1<-c(dfdpi,dfdb)
    df2<-df1%*%t(df1)
    df2
}

