"res.data.frame" <-
function(data)
{
 gene<-geneid.vect(data)
 stat1<-stat1.data.frame(data)
 stat2<-stat2.data.frame(data)
 stat2<-stat2[,-1]
 cbind(cbind(gene,stat1),stat2)
}

