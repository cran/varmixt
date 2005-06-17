"nmatch.an.anova" <-
function(dat1,dat2,P=0.05)
{

  pval.1<-pval.anova.vect(dat1)
  status.1<-rep("unregulated",length(pval.1))
  status.1[pval.1<=P]<-"regulated"

  pval.2<-pval.anova.vect(dat2)
  status.2<-rep("unregulated",length(pval.2))
  status.2[pval.2<=P]<-"regulated"
  
  n.match<-sum(status.1==status.2)
  n.tot<-length(status.2)
  n.reg1<-sum(status.1=="regulated")
  n.reg2<-sum(status.2=="regulated")
  res.mat<-data.frame(matrix(c(status.1,status.2),ncol=2))
  gene.reg.1<-dat1$geneid[status.1=="regulated"]
  gene.reg.2<-dat2$geneid[status.2=="regulated"]
  in2.not1<-length(setdiff(gene.reg.2,gene.reg.1))
  in1.not2<-length(setdiff(gene.reg.1,gene.reg.2))
  in2.not1.perc<-100*in2.not1/n.reg2
  in1.not2.perc<-100*in1.not2/n.reg1
  
  data.frame(n.tot=n.tot,n.match=n.match,n.reg1=n.reg1,n.reg2=n.reg2,in2.not1=in2.not1,
             in1.not2=in1.not2,in2.not1.perc=in2.not1.perc,in1.not2.perc=in1.not2.perc)
}

