makeForward <- function(comps, 
                        slopeCandidates,
                        groupCandidates,
                        nrOfCombs,
                        allowSwop,
                        intDep,
                        fixEf, 
                        bsType,
                        keep,
                        ...)
{
  
  returnListRE <- returnListS <- NULL
  # ellipsis <- as.list(substitute(list(...)))
  
  if(is.null(comps$random) & is.null(comps$gamPart)){ 
    
    gr <- rep("(Intercept)",length(groupCandidates))
    names(gr) <- groupCandidates
    
    returnListRE <- if(!is.null(groupCandidates)) lapply(split(gr, 1:length(gr)),as.list)
    
    returnListS <- if(!is.null(fixEf)) lapply(as.list(fixEf),as.list)
    
  }else{
    
    returnListS <- if(!is.null(comps$gamPart) | !is.null(fixEf)) 
      forwardGam(comps$gamPart, fixEf=fixEf, bsType=bsType, keep=keep$fixed, ...)
    
    returnListRE <- if(!is.null(slopeCandidates) | !is.null(groupCandidates) | 
                           length(comps$random)>1 | allowSwop | intDep > 1) 
      forwardStep(cnms=comps$random, slopeCandidates, groupCandidates, 
                  nrOfCombs, allowSwop, intDep)
    
    # problem: allowSwop or intDep > 1 migth be true though there are no other possible models
  
  }

  
  
  return(list(gamPart=returnListS, random=returnListRE))
  
}