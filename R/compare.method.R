"compare.method" <-
function(data,pval=0.05)
{
 N1<-nrow(data$stat1)
 status.gene<-rep("not regulated",N1)
 status.anova<-rep("not regulated",N1)
 status.vm2<-rep("not regulated",N1)
 status.vm<-rep("not regulated",N1)
 
 status.gene[pval.gene.vect(data)<=pval]<-"regulated"
 status.anova[pval.anova.vect(data)<=pval]<-"regulated"
 status.vm2[pval.VM2.vect(data)<=pval]<-"regulated"
 status.vm[pval.VM.vect(data)<=pval]<-"regulated"
 
 status.mat<-data.frame(status.vm2,status.vm,status.anova,status.gene)
 print(list(
                table(data.frame(status.vm2,status.vm)),
                table(data.frame(status.vm2,status.anova)),
                table(data.frame(status.vm2,status.gene)),
                table(data.frame(status.anova,status.gene)),
                table(data.frame(status.vm,status.anova)),
                table(data.frame(status.vm,status.gene))
            )
       )
invisible(status.mat)
}

