"qqplot.var.vm.2" <-
function(data,i)
{
    groups.vect <- group.vect(data)
    vars<-var.gene.vect(data)
    df.v<-df.vect(data)
    vars2 <- vars[groups.vect == i]
    df2<-median(df.v[groups.vect == i])
    if(length(vars2)>5 & df2>0)
    {
       qqplot.gamma(vars2,df2, main = as.character(i))
    }
}

