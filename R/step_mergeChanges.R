mergeChanges <- function(initialParts, listParts)
{

  ### initial part
  
  orgS <- orgRE <- NULL
  
  if(!is.null(initialParts$gamPart)){
  
    vars <- initialParts$gamPart$fake.names
    sTerm <- vars%in%sapply(initialParts$gamPart$smooth.spec,function(x)x$term)
    nonS <- vars[!sTerm]
    haveS <- vars[sTerm] # should be at least of lenght = 1 , else a (g)lmer should be fitted
    sLabs <- makeS(initialParts$gamPart)
    
    orgS <- append(sLabs,nonS)
    
  }
  
  if(!is.null(initialParts$random)) orgRE <- initialParts$random
  
  if(is.null(listParts$gamPart) & is.null(listParts$random)) 
    return(list(random=orgRE,gamPart=orgS))  
  
  newRE <- lapply(listParts$random,function(r)list(random=r,gamPart=orgS))
  newS <- lapply(listParts$gamPart,function(s)list(random=orgRE,gamPart=s))

  resList <- append(newRE,newS)
  
  ### drop those models with the exact same configuration as the initial model
  
  resList <- resList[!sapply(resList,function(r)
    is.logical(all.equal(r, list(random=orgRE,gamPart=orgS))))]
  
  return(resList)
  
}