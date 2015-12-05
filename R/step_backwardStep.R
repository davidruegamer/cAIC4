backwardStep <- function(cnms, keep)
{
    
  if( (sum(sapply(cnms,length))==1# & !isGam
       ) ){
    if(!is.null(keep)){
      retList <- vector("list",1)
      retList[[1]] <- cnms
      return(retList)
    }else{
      return(NA)
    }
  } 

  cnms2 <- rep(cnms,sapply(cnms,length))
  listCnms <- split(cnms2,names(cnms2))
  
  if(!is.null(keep)){
    
    keep <- interpret.random(keep)
    
    for(i in 1:length(listCnms)){
      
      if(names(listCnms)[i]%in%names(keep)){
        
        temp <- listCnms[[names(listCnms)[i]]] 
        indRem <- unlist(temp)!=unlist(keep)
        listCnms[[names(listCnms)[i]]] <- temp[indRem]

      }
    }
  } 
  
  newCnms <- lapply(listCnms,function(c){
    
    if(length(c)<=1){
      
#       if(names(c)%in%names(keep)){
#         keep[names(c)]
#       }else{
         list(NA)
#       }
      
    }else{
      
      for(i in 1:length(c)){
        
        c[[i]] <- c[[i]][-i]  
      
      }
#       if(names(c)%in%names(keep)){
#         append(c,keep[names(c)])
#       }else{
        c
#       }
      
    }
    
  })

  if(!is.null(keep)){
    
    for(n in names(keep)){
      
      if(is.na(newCnms[[n]])){
        newCnms[[n]] <- keep[n]
      }else{
        newCnms[[n]] <- append(newCnms[[n]],keep[n])
      }
    }
        
  }
  
  newCnms <- unlist(newCnms,recursive=FALSE)
  
  # problematic: variables with dots in name...
  names(newCnms) <- gsub("\\..*","",names(newCnms)) 
  
  listOfAllCombs <- vector("list",length(newCnms))
  
  for(i in 1:length(newCnms)){
    
    accessREi <- names(newCnms)[i]
    listOfAllCombs[[i]] <- append(cnms[names(cnms)!=accessREi],newCnms[i])
      
  }

  listOfAllCombs <- split(unlist(listOfAllCombs,recursive=FALSE),rep(1:length(newCnms),each=length(cnms)))
  listOfAllCombs <- lapply(listOfAllCombs,checkREs)
  
  listOfAllCombs <- listOfAllCombs[sapply(listOfAllCombs,function(r)!is.null(r))]
  listOfAllCombs <- lapply(listOfAllCombs,function(t)t[order(names(t))])
  listOfAllCombs <- listOfAllCombs[!(duplicated(listOfAllCombs) & duplicated(lapply(listOfAllCombs,names)))]

#   listOfAllCombs <- listOfAllCombs[
#     which(sapply(listOfAllCombs,function(l)
#     !is.logical(all.equal(l[order(names(l))],
#                           cnms[order(names(cnms))]))
#   ))
#   ]

  
  return(listOfAllCombs)
}