# family function for lme objects to have a generic function
# also working for lme models
family.lme <- function(object, ...)
{
  
  ret <- 
    list(family = "gaussian",
         link = "identity",
         linkfun = function(mu) mu,
         linkinv = function(eta) eta,
         variance = function(mu) rep(1, length(mu)),
         dev.resids = function(y, mu, wt) wt * ((y - mu)^2),
         aic = function(y, n, mu, wt, dev){
           nobs <- length(y)
           nobs * (log(dev/nobs * 2 * pi) + 1) + 2 - sum(log(wt))
         },
         mu.eta = function(eta) rep.int(1, length(eta)),
         initialize = expression({
           n <- rep.int(1, nobs)
           if (is.null(etastart) && is.null(start) && is.null(mustart) && 
               ((family$link == "inverse" && any(y == 0)) || (family$link == 
                                                              "log" && any(y <= 0)))) 
             stop("cannot find valid starting values: please specify some")
           mustart <- y
         }),
         validmu = function(mu) TRUE,
         valideta = function(eta) TRUE)
  class(ret) <- "family"
  
}