"remove.index" <-
function(data,index)
{
    for(i in 1:length(data))
    {   
        if(is.matrix(data[[i]]) | is.data.frame(data[[i]]))
        {
            data[[i]]<-data[[i]][-index,]
        }
        if(is.vector(data[[i]]))
        {
            if(length(data[[i]])>1)
            {
                data[[i]]<-data[[i]][-index]
            }
        }
    
    }
 data
}

