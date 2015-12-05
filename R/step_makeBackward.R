makeBackward <- function(comps, keep)
{
  
  # comps   list created by getComponents

  returnListRE <- returnListS <- NULL
  
  returnListRE <- if(!is.null(comps$random)) 
    backwardStep(comps$random, keep=keep$random)
  
  returnListS <- if(!is.null(comps$gamPart) && comps$gamPart$fake.formula[[3]]!=1) 
    backwardGam(comps$gamPart, keep=keep$fixed)
  
  return(list(gamPart=returnListS, random=returnListRE))
  
}