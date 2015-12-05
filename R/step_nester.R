nester <- function(namesGroups, intDep)
{
  
  # namesGroups <- unique(unlist(sapply(listOfRanefCombs,names)))
  nrOfColons <- countColons(namesGroups)
  newGroups <- namesGroups[nrOfColons<intDep-1]
  
  if(length(namesGroups)>1){
    
    combsGroups <- sapply(allCombn2(newGroups,intDep),paste0,collapse=":")
    combsGroups <- combsGroups[countColons(combsGroups)<=intDep-1]
    # newTerms <- as.list(rep("(Intercept)",length(combsGroups)))
    # names(newTerms) <- unlist(combsGroups)
    
#     orgLen <- length(listOfRanefCombs)
#     listOfRanefCombs <- append(listOfRanefCombs,vector("list",length(newTerms)))
#     
#     for(i in 1:length(newTerms)){ 
#       listOfRanefCombs[[orgLen+i]] <- newTerms[i]
#     }

  namesGroups <- append(namesGroups,combsGroups)
      
  }
  
  return(namesGroups)
  
}

countColons <- function(strings)
{
  
  sapply(regmatches(strings, gregexpr(":", strings)), length)
  
}