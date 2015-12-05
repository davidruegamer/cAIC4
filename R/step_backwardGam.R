backwardGam <- function(intGam, keep)
{
  
  # intGam    result of interpret.gam - call / $gamPart of getComponents-result

  vars <- intGam$fake.names
  sTerm <- vars%in%sapply(intGam$smooth.spec,function(x)x$term)
  nonS <- maybeNonS <- vars[!sTerm]
  addNonS <- addSlabs <- NULL
  haveS <- replaceHaveS <- vars[sTerm]  # should be at least of length = 1 , 
                        # else a (g)lmer should be fitted
  sLabs <- maybeSlabs <- makeS(intGam)

  # handle keep
  
  if(!is.null(keep)){
    
    keep <- interpret.gam(keep)
    keepVars <- keep$fake.names
    keepSterm <- keepVars%in%sapply(keep$smooth.spec,function(x)x$term)
    keepNonS <- keepVars[!keepSterm]
    keepHaveS <- keepVars[keepSterm]
    
    # prevent keepNonS variables to be excluded
    
    keepTermNonS <- nonS%in%keepNonS
    maybeNonS <- nonS[!keepTermNonS]
    addNonS <- nonS[keepTermNonS]
    
    # prevent keepHaveS variables to be excluded
    
    keepTermHaveS <- haveS%in%keepHaveS
    maybeSlabs <- sLabs[!keepTermHaveS]
    addSlabs <- sLabs[keepTermHaveS]
    replaceHaveS <- haveS[!keepTermHaveS]
    
  }

  
  returnList <- NULL
  
  # create list for dropping linear term
  
  if(length(maybeNonS)>0)
     returnList <- lapply(combn(maybeNonS,length(maybeNonS)-1,simplify=F),
                          function(x)append(x,c(sLabs,addNonS)))
  
  if(length(maybeSlabs)>0){
  
    for(i in 1:length(maybeSlabs)){
      
       temp <- maybeSlabs
       temp[i] <- replaceHaveS[i]
       returnList <- append(returnList,list(c(temp,addSlabs,nonS)))
      
    }
    
  }
  
  returnList
  
}