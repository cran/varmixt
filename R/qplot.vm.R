"qplot.vm" <-
function(data,lambda=seq(0,0.95,0.05))
{
 qplot(qvalue(pval.VM.vect(data),lambda=lambda))
}

