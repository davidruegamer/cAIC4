cnmsConverter <- function(cnms)
{
  
  charForm <- character(length(cnms))
  
  if(all(sapply(cnms,function(x) all(is.na(x)))) | all(sapply(cnms,is.null))) return(NULL)
  
  for(i in 1:length(cnms)){
    
    if ("(Intercept)"%in%cnms[[i]][1]) {
      cnms[[i]][which(cnms[[i]]=="(Intercept)")] <- "1"
    }else{
      cnms[[i]] <- append(cnms[[i]],"0")
    }
    
    
    
    charForm[i] <- paste("(", paste(cnms[[i]], 
                                    collapse = " + "), 
                         " | ", names(cnms)[i], ")", 
                         sep = "")
  }
  
  charForm
  
}