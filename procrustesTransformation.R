#This function creates a transformation of embeddings
#embeddings_per_time is a list of embedding matrices list(matrix(),matrix(),...)
#skip is the time window amongst the transformed matrices should be averaged
#- Transform basis is always the actual i'th vector matrix
#Procrustes is very usable for this task because it does not require any reference word selection
createProcrustes <- function(embeddings_per_time, skip = 1)
{
  res <- lapply(1:(length(embeddings_per_time)-skip), function(i)
  {
    #map on A regarding B -- B = t_1, A = t_+1
    all_sum <- matrix(0,nrow=nrow(embeddings_per_time[[i]]),ncol=ncol(embeddings_per_time[[i]]))
    
    for(j in 1:skip)
    {
      all_sum <- all_sum + pracma::procrustes(A=embeddings_per_time[[skip+i]],B=embeddings_per_time[[i+j-1]])$P
    }
    
    all_sum/skip
    
    })

    res <- createProcrustes(my_read_embeddings) #caution - this item does not exist

    #Calculation of the distances each word has to an orignal embedding matrix after transformation
    #More differnt == More semantical dynamics

    rows <- lapply(idx,function(x){
        unlist(lapply((skip+1):(length(my_read_embeddings)), function(i)
        {
        print(i)
        proxy::dist(rbind((res[[i-skip]])[x,,drop=F],(my_read_embeddings[[i]])[x,,drop=F]),method = "cosine")
        }))
    })
  
 
    #extract certain words from rows example 
    df <- do.call(rbind, rows)
  
    #DF needs rownames to be assigned
    return(df)
}

#The DF could be printed as timeline
#It would be possible to also compare differnt content parts within text etc.

