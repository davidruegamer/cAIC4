allCombn <- function(x,simplify=F)
{
  
  m <- length(x)-1
  unlist(lapply(X=1:m,function(i)combn(x,i,simplify=simplify)),recursive=F)
  
}

allCombn2 <- function(x,range,simplify=F)
{
  
  unlist(lapply(X=2:range,function(i)combn(x,i,simplify=simplify)),recursive=F)
  
}