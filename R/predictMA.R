predictMA <- function(object, new.data, ...){
  z <- object
  c <- z$candidatmodels
  w <- z$optimresults$weights
  pmodels <- sapply(z$candidatmodels, predict, newdata = new.data)
  MApredict <- w%*%t(sapply(c, predict, newdata = new.data))
  res <- list(prediction = MApredict, weights = w)
  return(res)
}
