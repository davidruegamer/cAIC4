forwardStep <- function(cnms,
                        slopeCandidates,
                        groupCandidates,
                        nrOfCombs,
                        allowSwop,
                        intDep)
{

  if(allowSwop){
      
      allSlopes <- unique(c(unlist(cnms),slopeCandidates),"(Intercept)")  
      
  }else{
        
      allSlopes <- c(slopeCandidates,"(Intercept)")
    
  }
  
  allGroups <- unique(c(names(cnms),groupCandidates))
  if(intDep>1) allGroups <- nester(allGroups, intDep)
        
  allSlopeCombs <- list()
      
  for(i in 1:nrOfCombs){
  
    if(i<=length(allSlopes)){
        
      allSlopeCombs <- append(allSlopeCombs,combn(allSlopes,m=i,simplify=FALSE))
        
    }
      
  }
      
  allSlopeCombs <- allSlopeCombs[sapply(allSlopeCombs,function(x)!any(duplicated(x)))]
         
  reList <- rep(allSlopeCombs,each=length(allGroups))
  names(reList) <- rep(allGroups,length(allSlopeCombs))
        
  allCombs <- lapply(X=1:length(reList),function(i)append(cnms,reList[i]))
  allCombs <- lapply(allCombs,checkREs)
  allCombs <- allCombs[!duplicated(allCombs)]
  allCombs <- allCombs[!(sapply(allCombs,function(x)all.equal(x,cnms))=="TRUE")]
 
  if(length(allCombs)==0) return(NULL)
 
  allCombs <- allCombs[sapply(allCombs,function(r)!is.null(r))]
  allCombs <- lapply(allCombs,function(t)t[order(names(t))])
  allCombs <- allCombs[!(duplicated(allCombs) & duplicated(lapply(allCombs,names)))]
     
  return(#list(randomPart=
     allCombs#, sPart=...)
  )
  
}