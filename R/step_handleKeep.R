sepKeeps <- function(comps, keep=keep)
{
  
  keepRE <- keep$random
  keepS <- keep$fixed
  
  if(!is.null(keepS)) keepS <- interpret.gam(keepS)
  if(!is.null(keepRE)) keepRE <- interpret.random(keepRE)
  
  randomNK <- excludeRE(comps,keepRE)
  gamPartNK <- excludeS(comps,keepS)   
  
  return(list(random=random,
              gamPart=gamPart))
  
}

addKeeps <- function(keep, newComps)
{

  random <- lapply(newComps$random,function(x)append(x,keep$random))
  gamPart <- lapply(newComps$gamPart,function(x)append(x,keep$gamPart))
  
  return(list(random=random,
              gamPart=gamPart))
  
}

exludeRE <- function(org,excl)
{
  
  # search names, seach slopes ... no.... 
  
}

excludeS <- function(org,excl)
{
  
  
  
}