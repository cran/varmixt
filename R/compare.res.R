"compare.res" <-
function(data1,data2,pval=0.05)
{
 N1<-n.genes(data1)
 N2<-n.genes(data2) 
  
 geneids<-unique(c(geneid.vect(data1),geneid.vect(data2)))
 N<-length(geneids)
 data1.to.res<-match(geneid.vect(data1),geneids)
 data2.to.res<-match(geneid.vect(data2),geneids)
 
 status.VM2.data10<-rep("not regulated",N1)
 status.VM2.data10[pval.VM2.vect(data1)<=pval]<-"regulated"
 status.VM2.data1<-rep("not present",N)
 status.VM2.data1[data1.to.res]<-status.VM2.data10

 status.VM.data10<-rep("not regulated",N1)
 status.VM.data10[pval.VM.vect(data1)<=pval]<-"regulated"
 status.VM.data1<-rep("not present",N)
 status.VM.data1[data1.to.res]<-status.VM.data10

 status.anova.data10<-rep("not regulated",N1)
 status.anova.data10[pval.anova.vect(data1)<=pval]<-"regulated"
 status.anova.data1<-rep("not present",N)
 status.anova.data1[data1.to.res]<-status.anova.data10

 status.gene.data10<-rep("not regulated",N1)
 status.gene.data10[pval.gene.vect(data1)<=pval]<-"regulated"
 status.gene.data1<-rep("not present",N)
 status.gene.data1[data1.to.res]<-status.gene.data10

 group.data10<-group.vect(data1)
 group.data1<-rep("not present",N)
 group.data1[data1.to.res]<-group.data10


 status.VM2.data20<-rep("not regulated",N2)
 status.VM2.data20[pval.VM2.vect(data2)<=pval]<-"regulated"
 status.VM2.data2<-rep("not present",N)
 status.VM2.data2[data2.to.res]<-status.VM2.data20

 status.VM.data20<-rep("not regulated",N2)
 status.VM.data20[pval.VM.vect(data2)<=pval]<-"regulated"
 status.VM.data2<-rep("not present",N)
 status.VM.data2[data2.to.res]<-status.VM.data20

 status.anova.data20<-rep("not regulated",N2)
 status.anova.data20[pval.anova.vect(data2)<=pval]<-"regulated"
 status.anova.data2<-rep("not present",N)
 status.anova.data2[data2.to.res]<-status.anova.data20

 status.gene.data20<-rep("not regulated",N2)
 status.gene.data20[pval.gene.vect(data2)<=pval]<-"regulated"
 status.gene.data2<-rep("not present",N)
 status.gene.data2[data2.to.res]<-status.gene.data20

 group.data20<-group.vect(data2)
 group.data2<-rep("not present",N)
 group.data2[data2.to.res]<-group.data20
 
 geneid<-geneids
 
 comp.status<-data.frame(status1.VM2=status.VM2.data1,status1.VM=status.VM.data1,status1.anova=status.anova.data1,status1.gene=status.gene.data1,
                         group.data1=group.data1, status2.VM2=status.VM2.data2,status2.VM=status.VM.data2,
                         status2.anova=status.anova.data2, status2.gene=status.gene.data2,group.data1=group.data1,
                         group.data2=group.data2, geneid=geneid)

 cat("------------------------------------------------------------------------\n")
 cat("------------------------------------------------------------------------\n")
 print(table(comp.status[,c("status1.VM2","status2.VM2")]))
 cat("------------------------------------------------------------------------\n")
 cat("------------------------------------------------------------------------\n")
 print(table(comp.status[,c("status1.VM","status2.VM")]))
 cat("------------------------------------------------------------------------\n")
 cat("------------------------------------------------------------------------\n")
 print(table(comp.status[,c("status1.anova","status2.anova")]))
 cat("------------------------------------------------------------------------\n")
 cat("------------------------------------------------------------------------\n")
 print(table(comp.status[,c("status1.gene","status2.gene")]))
 cat("------------------------------------------------------------------------\n")
 cat("------------------------------------------------------------------------\n")
 print(table(comp.status[,c("group.data1","group.data2")]))
 cat("------------------------------------------------------------------------\n")
 cat("------------------------------------------------------------------------\n")
 invisible(comp.status)
}
