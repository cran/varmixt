"build.data.paired" <-
function(name,ratio,meanint=NULL,badqual=NULL,gene.anot=NULL,min.rep=2)
{
    n.g<-length(name)
    n.r<-nrow(ratio)
    if(n.g!=n.r){stop("length of gene name is not equal to nrow of ratio matrix")} 

    min.rep<-max(min.rep,2)
    glob.ind<-apply(ratio,1,FUN=function(x){sum(is.finite(x))>=min.rep} )

    if(!all(glob.ind)){warning(sum(!glob.ind)," genes with too many missing values were removed")}

    res<-list(geneid=name[glob.ind],raw.ratio=ratio[glob.ind,])
    if(!is.null(meanint))
    {
        res$raw.meanint<-meanint[glob.ind,]
        n.m<-nrow(meanint)
        if(n.g!=n.m){stop("length of gene name is not equal to nrow of meanint matrix")} 
    }
    
    if(!is.null(badqual))
    {
        n.b<-nrow(badqual)
        badqual.sum<-apply(badqual[glob.ind,],1,FUN=function(x)sum(x[is.finite(x)]))   
        res$badqual<-badqual[glob.ind,]
        res$global.qual<-badqual.sum
        if(n.g!=n.b){stop("length of gene name is not equal to nrow of badqual matrix")} 
   }
   if(!is.null(gene.anot))
   {
        n.ge<-nrow(gene.anot)
        res$gene.anot<-gene.anot[glob.ind,]
        if(n.g!=n.ge){stop("length of gene name is not equal to nrow of gene.anot matrix")}
   }
   
   class(res)<-"madata"
   res
}

