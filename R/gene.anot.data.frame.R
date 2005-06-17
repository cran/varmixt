"gene.anot.data.frame" <-
function(data)
{
    if(!is.null(data$gene.anot))
    {
     res<-as.data.frame(data$gene.anot)
    }
    else
    {
     res<-NULL
    }
    res
}

