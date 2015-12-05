forwardGam <- function(intGam, fixEf=NULL, bsType="ps", keep)
{

  vars <- intGam$fake.names
  sTerm <- vars%in%sapply(intGam$smooth.spec,function(x)x$term)
  nonS <- vars[!sTerm]
  haveS <- vars[sTerm] # should be at least of lenght = 1 , else a (g)lmer should be fitted
  sLabs <- makeS(intGam)
  keepNonS <- NULL

  newX <- fixEf[which(!fixEf %in% vars)]
  
  if(!is.null(keep)){
    
    keep <- interpret.gam(keep)
    keepVars <- keep$fake.names
    keepSterm <- keepVars%in%sapply(keep$smooth.spec,function(x)x$term)
    keepNonS <- keepVars[!keepSterm]
    
    nonS <- nonS[!nonS%in%keepNonS] # drop the keepNonS from nonS
    # to prevent s-making
    if(length(nonS)==0) nonS <- NULL
    
  }
  
  returnListS <- vector("list",length=length(nonS)+length(newX)) 
  
  for(i in 1:length(returnListS)){
    
    if(i <= length(newX)){
      
      returnListS[[i]] <- c(keepNonS,sLabs,nonS,newX[i])
      
    }else{
      
      returnListS[[i]] <- c(keepNonS,sLabs, 
                            paste0("s(",nonS[i-length(newX)],
                                   ",bs=",deparse(bsType),")"))
      
    }
    
  }
  
  returnListS
  
}