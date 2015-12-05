makeFormula <- function(setup, modelInit)
{
  
  # setup         list ($random,$gamPart) created by makeBackward / makeForward
  # modelInit     initial model
    
  # get config
  
  isGam <- !is.null(setup$gamPart) & length(setup$gamPart)>0
  wasGam <- is.list(modelInit) & length(modelInit)==2 & all(c("mer","gam") %in% names(modelInit))
  
  random <- gamPart <- reFormula <- rhs <-  NULL
  
  ### create random part
  
  if(!is.null(setup$random) && all(!is.na(setup$random))){
  
    charForm <- cnmsConverter(setup$random)
    
    reFormula <- paste(charForm, collapse = " + ")
  
  }
  
  ### create gamPart / lhs / rhs
  
  if(isGam){
    
    rhs <- paste(setup$gamPart, collapse = " + ")    
    
  }else{
    
    if(wasGam){
      
      rhs <- "1"
      
    }else{ # (g)lmer / (g)lm
    
      if(nobars(formula(modelInit)) == formula(modelInit)[[2]]){
        
        nobarsF <- NULL
        
      }else{
        
        nobarsF <- attr(terms(nobars(formula(modelInit))), "term.labels")
        
      }
      
      rhs <- c(nobarsF, reFormula)
        
    }
    
  }

  # if there are no covariates, set rhs to "1"
  
  if(is.null(rhs) | length(rhs)==0) rhs <- "1"
  
  # extract response
  
  lhs <- ifelse(wasGam, formula(modelInit$gam)[[2]], formula(modelInit)[[2]])
  
  # specify the parts random and gamPart
  
  if(isGam | wasGam){
    
      random <- reFormula
      gamPart <- reformulate(rhs, lhs)
    
    }else{
      
      random <- reformulate(rhs, lhs)
          
  }

  return(list(random=random,
              gamPart=gamPart)
         )

  
}