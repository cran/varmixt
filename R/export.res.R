"export.res" <-
function(data,filename=NULL,header=TRUE,comment=NULL,lambda=seq(0,0.95,0.05))
{
 geneid<-gene.id.vect(data)
 
 stat1<-stat1.data.frame(data) 
 stat2<-stat2.data.frame(data)[,-1]
 
 temp<-cbind(I(geneid),stat1)
 temp<-cbind(temp,stat2)
 
 if (!is.null(qual.vect(data)))
 {
  temp<-cbind(temp,qual=qual.vect(data))
 }
 if(!is.null(gene.anot.data.frame(data)))
 {
    names.anot<-names(gene.anot.data.frame(data))
    if(is.null(names.anot))
    {
        names.anot<-vector()
        for(k in 1:length(gene.anot.data.frame(data)))
        {
            names.anot[k]<-paste("gene.anot",i,sep=".")
        }
    }
 }

 
 temp$ratio       <-exp(log(2)*logratio.vect(data))

 temp$rank.p.VM2  <-rank(pval.VM2.vect(data))
 temp$qval.VM2    <-qval.VM2.vect(data,lambda=lambda)
 

 temp$rank.p.VM   <-rank(pval.VM.vect(data))
 temp$qval.VM     <-qval.VM.vect(data,lambda=lambda)
 
 temp$rank.p.anova<-rank(pval.anova.vect(data))
 temp$qval.anova  <-qval.anova.vect(data,lambda=lambda)

 temp$rank.p.gene <-rank(pval.gene.vect(data))
 temp$qval.gene   <-qval.gene.vect(data,lambda=lambda)

 temp$df          <-df.vect(data)
 
 col.ids<-c("geneid","ratio","df","pval.VM2","qval.VM2","corrected.pval.VM2","test.stat.VM2","sigmadeltagVM2","group","rank.p.VM2",
            "rank.p.VM","rank.p.gene", "rank.p.anova","pval.gene","qval.gene","corrected.pval.gene","test.stat.gene","sigmadeltag",
            "varg","pval.anova","qval.anova", "corrected.pval.anova", "test.stat.anova","sigmadeltaanova","pval.VM","qval.VM",
            "corrected.pval.VM","test.stat.VM", "sigmadeltagVM", "rank.VM2","rank.gene","rank.anova", 
            "rank.VM","deltag","meanint")
 
 if(!is.null(temp$group))
 {
    if(is.null(qual.vect(data)))
    {
        temp<-temp[,col.ids]

    }
    if (!is.null(qual.vect(data)))
    {
        ind1<-match("ratio",col.ids)
        col.ids.2<-c(col.ids[1:ind1],"qual",col.ids[(ind1+1):length(col.ids)])    
        temp<-temp[,col.ids.2]

    }
 }
 if(!is.null(temp$group.treat) & !is.null(temp$group.cont))
 {
   ind1<-match("group",col.ids)
   col.ids.2<-c(col.ids[1:(ind1-1)],"group.cont","group.treat",col.ids[(ind1+1):length(col.ids)])

    if (is.null(qual.vect(data)))
    {
        temp<-temp[,col.ids.2]

    }
    if (!is.null(qual.vect(data)))
    {
        ind1<-match("ratio",col.ids.2)
        col.ids.3<-c(col.ids.2[1:ind1],"qual",col.ids.2[(ind1+1):length(col.ids.2)])    
        temp<-temp[,col.ids.3]
    }
 }
 if(!is.null(gene.anot.data.frame(data)))
 {
    name.1<-names(temp)
    temp<-cbind(temp,gene.anot.data.frame(data))
    names(temp)<-c(name.1,names.anot)
 }
 if(!is.null(filename))
 {
     if(header)
     {
         cat(format(Sys.time(), "%a %b %d %Y; %X "),"\n",deparse(data$call),"\n",comment,"\n",file=filename)
         write.table(file=filename,x=temp,sep="\t",row.names=FALSE,append=TRUE)
     }
     else
     {
         write.table(file=filename,x=temp,sep="\t",row.names=FALSE)
     }
 }
 comment(temp)<-c(date()," ",deparse(match.call())," ",comment)
 invisible(temp)
}
