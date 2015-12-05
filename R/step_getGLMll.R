getGLMll <- function(object)
{
  
  y <- object$y
  if(is.null(y)) y <- eval(object$call$data, environment(formula(object)))[all.vars(formula(object))[1]][[1]]
  if(is.null(y)) stop("Please specify the data argument in the initial model call!")
  
  mu <- predict(object,type="response")
  sigma <- ifelse("glm"%in%class(object),sqrt(summary(object)$dispersion),summary(object)$sigma)  
  
    switch(family(object)$family, binomial = {
    cll <- sum(dbinom(x = y, size = length(unique(y)), prob = mu, log = TRUE))
  }, poisson = {
    cll <- sum(dpois(x = y, lambda = mu, log = TRUE))
  }, gaussian = {
    cll <- sum(dnorm(x = y, mean = mu, sd = sigma, log = TRUE))
  }, {
    cat("For this family no bias correction is currently available \n")
    cll <- NA
  })
  return(cll)
  
  
}