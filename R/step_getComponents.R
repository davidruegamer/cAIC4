getComponents <- function(object)
{
  
  isGam <- is.list(object) & length(object)==2 & 
    all(c("mer","gam") %in% names(object))
  gamPart <- NULL
  random <- NULL
  
  if(isGam){
    
    nrOfSmooths <- length(object$gam$smooth)
    # cutp <- length(object$mer@cnms)-nrOfSmooths
    random <- object$mer@cnms[!object$mer@cnms%in%sapply(object$gam$smooth,function(x)x$label)] # ,cutp)
    if(length(random)==0) random=NULL
    
    gamPart <- interpret.gam(object$gam$formula)
    
  }else if(inherits(object, c("lmerMod", "glmerMod"))){
    
    random <- object@cnms
    
  }#else if(any(class(object)%in%c("lm","glm"))){
            
  #}  
  
  return(list(random=random,
              gamPart=gamPart
              )
         )
  
}