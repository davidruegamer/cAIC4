checkHierarchicalOrder <- function(listIn)
{
  
  listIn <- listIn[order(sapply(listIn,length),decreasing=T)]
  
  notAllowed <- list()
  lenMax <- length(listIn[[1]])
  lenMin <- length(listIn[[length(listIn)]])
  
  i = 1
  
  while(i < length(listIn)){

    lenI <- length(listIn[[i]])
    
    if( lenMax>1 & lenI>lenMin ){ 
      
      notAllowed <- allCombn(listIn[[i]])
      listIn <- listIn[!listIn%in%notAllowed]
      
    }
    
    i = i + 1
    
  }

  return(listIn)  
  
}