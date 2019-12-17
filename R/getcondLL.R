#' Function to calculate the conditional log-likelihood
#' 
#' @param object An object of class \code{merMod} either fitted by
#' \code{\link[lme4]{lmer}} or \code{\link[lme4]{glmer}} of the 'lme4' package.
#' 
#' @return conditional log-likelihood value
#' @importFrom stats weights
#' @export
getcondLL <- function(object) UseMethod("getcondLL")

#' @return \code{NULL}
#'
#' @rdname getcondLL
#' @importFrom nlme getResponse
#' @export 
getcondLL.lme <-
  function(object) {
    stopifnot(family(object)$family == "gaussian")
    y <- as.vector(getResponse(object))
    y_hat <- predict(object) # re at their predicted values
    R <- as.matrix(get_R(object))
    sum(mvtnorm::dmvnorm(x = y, mean = y_hat, sigma = R, log = TRUE))
  }

#' @return \code{NULL}
#'
#' @rdname getcondLL
#' @export 
getcondLL.merMod <-
function(object) {
  # A function that calls the bias correction functions.
  #
  # Args: 
  #   object = Object of class lmerMod or glmerMod. Obtained by lmer() or glmer()
  #
  # Returns:
  #   cll    = The conditional log-likelihood.
  #
  
  # check for weights
  w <- weights(object)
  if(sum(w)!=length(w)){
    if(family(object)$family != "gaussian") 
      warning("Weights for family != gaussian not implemented yet.")
  }
  
  switch(family(object)$family,
    binomial = {
      cll <- sum(dbinom(x    = getME(object, "y"), 
                        size = length(unique(getME(object, "y"))) - 1, 
                        prob = getME(object, "mu"), log = TRUE))
    },
    poisson  = {
      cll <- sum(dpois(x      = getME(object, "y"), 
                       lambda = getME(object, "mu"), log = TRUE))
    },
    gaussian = {
      cll <- sum(dnorm(x    = getME(object, "y"), 
                       mean = getME(object, "mu"), 
                       sd   = sigma(object) / sqrt(w), log = TRUE))
    },
    {
      cat("For this family no bias correction is currently available \n")
      cll <- NA
    }
    )
  return(cll)
}
