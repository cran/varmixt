"qqplot.var.vm" <-
function(data)
{
    groups.vect <- group.vect(data)
    vars<-var.gene.vect(data)
    df.v<-df.vect(data)
    ngroups <- length(unique(groups.vect))
    n.rows <- 2
    n.cols <- ceiling(ngroups/n.rows)
    if ((n.rows*n.cols-length(unique(groups.vect)))==1){
    par(mfrow=c(n.rows,n.cols))
    }
    if ((n.rows*n.cols-length(unique(groups.vect)))==0){
    par(mfrow=c(n.rows,n.cols+1))
    }
     df<-median(df.v)
     qqplot.gamma(vars,df, main = "all")
    for (i in sort(unique(groups.vect))) 
    {
        vars2 <- vars[groups.vect == i]
        df2<-median(df.v[groups.vect == i])
        if(length(vars2)>5) 
        {
            qqplot.gamma(vars2,df2, main = as.character(i))
        }
    }
}
