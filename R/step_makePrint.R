makePrint <- function(comps, initial=TRUE)
{
  
  if(initial){
    
    isLMER <- FALSE
    
    if(inherits(comps, c("lmerMod", "glmerMod"))){

      f <- nobars(formula(comps))
      f <- if(is.name(f)){
              1 
            }else{
              f[[length(f)]]
              }
      
      isLMER <- TRUE
      
    }
    
    comps <- mergeChanges(getComponents(comps), NULL)
    
    if(isLMER) comps$gamPart <- all.vars(f)    
    
    gp <- c(
      comps$gamPart,
      cnmsConverter(comps$random)
    )
    
    if(is.null(gp) | length(gp)==0) gp <- "1"
    
    pr <- paste0("~ ",
           paste(gp,
           collapse = " + ")
    )
    
  }else{
    
    gp <- NULL
    gp <- if(!is.null(comps$gamPart)) as.character(Reduce(paste,deparse(comps$gamPart)))
    parts <- c(gp,comps$random)
    # print(parts)
    pr <- paste(parts,collapse=" + ")    
    pr <- as.character(Reduce(paste,deparse(as.formula(pr)[-2]))) # too complicated
    
  }
  
  return(pr)
      
}