"prt.varmixt.res" <-
function(data)
{
    param<-parameters.data.frame(data)
    n.mixt<-param[1,2]
    pis<-param[param[,1]=="pi",2]
    vars<-param[param[,1]=="sigma2",2]
    sds<-sqrt(vars)
    format.res<-format(c(pis,sds,vars),digits=1,nsmall=4)
    pis.f<-format.res[1:n.mixt]
    sds.f<-format.res[(n.mixt+1):(2*n.mixt)]
    vars.f<-format.res[(2*n.mixt+1):(3*n.mixt)]
    cat(deparse(data$call),"\n")
    cat("Varmixt version 0.2-2 \n")
    cat("Total number of gene probes=",n.genes(data),"\n")
    cat("Number of components=",n.mixt,"\n")
    cat(" pi=",pis.f,"\n")
    cat(" sd=",sds.f,"\n")
}
