"fdr.compare.method" <-
function(data,qval=0.05,display.all=FALSE,lambda=seq(0,0.95,0.05))
{
 N1<-nrow(data$stat1)
 
 limits<-fdr.an(data,Q=qval,display=FALSE,lambda=lambda)
 pval.vm2<-limits$pval.vm2
 pval.vm<-limits$pval.vm
 pval.anova<-limits$pval.anova
 pval.gene<-limits$pval.gene
 
 status.gene<-rep("not regulated",N1)
 status.anova<-rep("not regulated",N1)
 status.vm2<-rep("not regulated",N1)
 status.vm<-rep("not regulated",N1)

 
 status.gene[pval.gene.vect(data)<=pval.gene]<-"regulated"
 status.anova[pval.anova.vect(data)<=pval.anova]<-"regulated"
 status.vm2[pval.VM2.vect(data)<=pval.vm2]<-"regulated"
 status.vm[pval.VM.vect(data)<=pval.vm]<-"regulated"
 
 status.mat<-data.frame(status.vm,status.vm2,status.anova,status.gene)
 
 cat("--------------------------------------------\n")
 cat("--------------------------------------------\n")
 print(table(data.frame(status.vm2,status.vm)))
 cat("--------------------------------------------\n")
 cat("--------------------------------------------\n")
 if(display.all)
 {
    print(table(data.frame(status.vm2,status.anova)))
    cat("--------------------------------------------\n")
    cat("--------------------------------------------\n")
    print(table(data.frame(status.vm2,status.gene)))
    cat("--------------------------------------------\n")
    cat("--------------------------------------------\n")
    print(table(data.frame(status.vm,status.anova)))
    cat("--------------------------------------------\n")
    cat("--------------------------------------------\n")
    print(table(data.frame(status.vm,status.gene)))
    cat("--------------------------------------------\n")
    cat("--------------------------------------------\n")
    print(table(data.frame(status.anova,status.gene)))
    cat("--------------------------------------------\n")
    cat("--------------------------------------------\n")
 }
 invisible(status.mat)
}

