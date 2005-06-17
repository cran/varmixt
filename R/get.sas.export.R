"get.sas.export" <-
function(file.test,file.cont,file.qual)
{
    testdat<-read.table(file=file.test,header=T)
    geneID<-as.character(testdat[[1]])
    testdat<-testdat[,-c(1,2)]

    contdat<-read.table(file=file.cont,header=T)
    contdat<-contdat[,-c(1,2)]
 
    qualdat<-read.table(file=file.qual,header=T)
    qualdat<-qualdat[,-c(1,2)]
    
    list(geneID=geneID,treat=testdat,cont=contdat,badqual=qualdat)
}

