"find.gene.index" <-
function(data,gene)
{

 res<-which(geneid.vect(data)==gene)
 res
}

