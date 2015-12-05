interpret.random <- function(frla)
{
  
  bars <- findbars(frla[[length(frla)]])
  names(bars) <-  unlist(lapply(bars, function(x) deparse(x[[3]])))
  
  lapply(bars, function(b){
    
    hasInt <- attr(terms(as.formula(paste0("~",deparse(subbars(b))))),"intercept")==1
        
    v <- ifelse(hasInt, "(Intercept)", "0")
    v <- append(v,all.vars(as.formula(paste0("~",deparse(b[[2]])))))
    
    
    return(v)
      
  })
  
}