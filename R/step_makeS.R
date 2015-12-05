makeS <- function(intGam)
{
  
    sapply(intGam$smooth.spec, 
           function(x) paste0("s(",
                              x$term,
                              ",bs=",
                              deparse(paste0(substring(attr(x,"class"), 1, 2))),
                              ")")
           )
    
}