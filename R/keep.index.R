"keep.index" <-
function(data,index)
{
    N<-n.genes(data)
    for(i in 1:length(data))
    {   
        if(is.matrix(data[[i]]) | is.data.frame(data[[i]]))
        {
            if(nrow(data[[i]])==N) data[[i]]<-data[[i]][index,]
        }
        
        if(is.vector(data[[i]]))
        {
            if(length(data[[i]])==N) data[[i]]<-data[[i]][index]
        }
    
    }
 data
}

