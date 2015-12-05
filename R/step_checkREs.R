checkREs <- function(reList)
{
  
  # first check: NULL-REs / NA-REs
  
  reList <- reList[sapply(reList,function(x)!is.null(x))]
  reList <- reList[sapply(reList,function(x)any(!is.na(x)))]
  
  nam <- unique(names(reList))
    
  checkedList <- list()
  
  for(i in 1:length(nam)){
        
    namL <- reList[names(reList)==nam[i]]
    namL <- lapply(namL,sort)    
        
    # second check: duplicated REs    
    namL <- namL[!duplicated(namL)]
    
    # third check: hierarchical order
    if(length(namL)>1) namL <- checkHierarchicalOrder(namL)
        
    checkedList <- append(checkedList,namL)
    
  }
  

  return(checkedList)  
  
}