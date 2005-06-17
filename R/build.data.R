"build.data" <-
function(name,cond1,cond2,badqual=NULL,gene.anot=NULL,min.rep=2)
{ 
   n.g<-length(name)
   n.c1<-nrow(cond1)
   n.c2<-nrow(cond2)
   if(n.g!=n.c1 | n.g!=n.c2){stop("length of gene name is not equal to nrow of cond1 or cond2 matrix")}
    
   min.rep<-max(min.rep,2)
   cond1.ind<-apply(cond1,1,FUN=function(x){sum(is.finite(x))>=min.rep} )
   cond2.ind<-apply(cond2,1,FUN=function(x){sum(is.finite(x))>=min.rep} )

   glob.ind <-(cond1.ind & cond2.ind)

   if(!all(glob.ind)){warning(sum(!glob.ind)," genes with too many missing values were removed")}
   
   res<-list(geneid=name[glob.ind],raw.cond1=cond1[glob.ind,],raw.cond2=cond2[glob.ind,])
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
