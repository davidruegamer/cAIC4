calculateAllCAICs <- function(newSetup,
                              # gamPos,
                              modelInit, 
                              numbCores, 
                              data, 
                              calcNonOptimMod, 
                              ...)
{

  #   if(is.null(newSetup$sPart)) isGam <- FALSE  

  formulaList <- lapply(newSetup,function(x)makeFormula(x,modelInit))
  
  ### create all possible models ###
  
  listOfModels <- mclapply(formulaList, function(f)
    makeUpdate(modelInit=modelInit, setup=f, data=data, ...),
    mc.cores=numbCores)

  
  #######################################################################
  ################### calculate alle the cAICs ##########################
  #######################################################################
  
  listOfCAICs <- lapply(mclapply(listOfModels,function(m){
    
    if(any(class(m)%in%c("glm","lm"))){
      
      ll <- getGLMll(m)
      bc <- attr(stats4:::logLik(m),"df")
      caic <- -2*ll + 2*bc
      c(ll,bc,caic)
      
    }else{
      
      if(class(m)=="list"){ # m is a gamm4 object
        
        cAIC(m,...)[c("loglikelihood","df","caic")]
        
      }else{
        
        if(!calcNonOptimMod){
          
          errCode <- m@optinfo$conv$lme4$code
          if(!is.null(errCode)) return(c(NA,NA,NA))
          
        }
        
        tryCatch(cAIC(m,...)[c("loglikelihood","df","caic")], error = function(e) return(c(NA,NA,NA)))
        
      }
      
    }},
    mc.cores=numbCores),unlist)
  
  #######################################################################
  ################ list all the cAICs and models ########################
  #######################################################################
  
    
  aicTab <- as.data.frame(as.matrix(do.call("rbind",listOfCAICs)))
  colnames(aicTab) <- c("loglikelihood","df","caic")
  aicTab$models <- sapply(formulaList, makePrint, initial=FALSE)

  aicTab <- as.data.frame(aicTab[,c("models","loglikelihood","df","caic")])
    
  minInd <- which.min(aicTab$caic)
  bestMod <- NA
  if(length(minInd)!=0) bestMod <- listOfModels[[minInd]]
  
  return(list(aicTab=aicTab,
              bestMod=bestMod)
  )
  
}