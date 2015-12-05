makeUpdate <- function(modelInit, 
                       setup, 
                       data, 
                       ...)
{

  willBeGam <- !is.null(setup$gamPart) & 
    grepl("s\\(",Reduce(paste,deparse(setup$gamPart))) # probably not the best way to check...

  hasBars <- ifelse(!is.character(setup$random),
                    !is.null(findbars(setup$random)),
                    !is.null(findbars(as.formula(paste0("~",setup$random))))
                    )
  
  isGlm <- any(class(modelInit)%in%c("glm","lm"))
  
  isGam <- is.list(modelInit) & length(modelInit)==2 & all(c("mer","gam") %in% names(modelInit))

  # make a decision which method should be used for fitting
  
  if(!willBeGam & isGlm & hasBars){
  
    fm <- ifelse(isGam,
                 family(modelInit$mer)$family,
                 family(modelInit)$family)
      
    mod <- if(fm=="gaussian"){
      
      lmer(setup$random, data = data, ...)
    
    }else{
                  
      glmer(setup$random, data = data, 
            family = fm, ...)
    
    }
    
  }else if(!willBeGam & !isGlm & hasBars & !isGam){
    
    mod <- update(modelInit,
                  formula = setup$random)    
    
  }else if(!willBeGam & !hasBars & !isGam){
    
    fm <- ifelse(isGam,
                 family(modelInit$mer)$family,
                 family(modelInit)$family)
    
    mod <- glm(setup$random, family=fm,
               data=data)
    
  }else if(!willBeGam & !hasBars & isGam){
    
    fm <- ifelse(isGam,
                 family(modelInit$mer)$family,
                 family(modelInit)$family)
    
    mod <- glm(setup$gamPart, family=fm,
               data=data)    
    
    
  }else{ # willBeGam
        
    r <- if(!is.null(setup$random)){
      as.formula(paste("~",setup$random))
    }else{
      NULL
    }
      
    mod <- gamm4(setup$gamPart, 
                 data = data, 
                 family = family(modelInit$mer), 
                 random = r, 
                 ...)
    
  }
  
  return(mod)
  
}