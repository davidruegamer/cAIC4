refitModelWithLinearTerm <- function(m, zeroterm)
{
  
  # object: mer part of gamm4 model with additional info extracted in cAIC
  # zeroterm: character specifying the s-term which has zero variance
  
  # TODO: add check for correlated errors
  if(m@optinfo$gammnulldim[zeroterm]==1){
    linTerm <- gsub("s\\((.*)\\)","\\1", zeroterm)
    form <- gsub(paste0("s\\(", linTerm, ".*\\)"), linTerm, as.character(m@optinfo$gammform)[3])
    cat("Refitting model with zero variance term and null space dimension 1.")
    object <- gamm4(reformulate(form, as.character(m@optinfo$gammform)[2]),
                    data = m@optinfo$gammdata)
  }else{
    
    stop("After removing the terms with zero variance components and refitting 
          the model cAIC can be called again.", call. = FALSE)
    
  }
  
  gammform <- object$gam$formula
  gammnulldim <- sapply(object$gam$smooth, "[[", "null.space.dim")
  if(any(gammnulldim==1)){
    gammdata <- object$gam$model
  }
  names(gammnulldim) <- sapply(object$gam$smooth, "[[", "label")
  object <- object$mer
  object@optinfo$gamm4 <- TRUE    # add indicator for gamm4
  object@optinfo$gammform <- gammform
  object@optinfo$gammnulldim <- gammnulldim
  object@optinfo$gammdata <- gammdata
  
  return(object)
  
}