biasCorrectionBernoulli <- function(object){
  # A function that calculates the bias correction for the estimation of the Kullback-Leibler distance
  #
  # Args: 
  #   object = gam object with family=binomial
  #
  # Returns:
  #   bc = Bias correction for Binomial gam
  #
  zeroLessModel <- deleteZeroComponents(object)
  if (inherits(zeroLessModel, "glm")) {
    return(zeroLessModel$rank)
  }
  signCor 	 <- - 2 * zeroLessModel@resp$y + 1
  muHat   	 <- zeroLessModel@resp$mu
  workingEta <- numeric(length(muHat))
  for(i in 1:length(muHat)){
  	workingData 	<- zeroLessModel@resp$y
  	workingData[i] 	<- 1 - workingData[i]
  	workingModel 	<- refit(zeroLessModel, nresp = workingData)
  	workingEta[i] 	<- log(workingModel@resp$mu[i] / (1 - workingModel@resp$mu[i])) - log(muHat[i] / (1 - muHat[i]))
  }
  bc <- sum(muHat * (1 - muHat) * signCor * workingEta)
	if (identical(object, zeroLessModel)) {
      newModel <- NULL
      new      <- FALSE
    } else {
      newModel <- zeroLessModel
      new      <- TRUE
    }
	return(list(bc = bc, newModel = newModel, new = new))
}

#
#biasCorrectionBernoulli <- function(object) {
#  # A function that calculates the bias correction for a generalized linear 
#  # mixed models with binary(!) data similar to the centralized Steinian method
#  # in Efron (2004).
#  #
#  # Args: 
#  #   object = Object of class lmerMod or glmerMod. Obtained by lmer() or 
#  #            glmer(). Needs binary data.
#  #
#  # Returns:
#  #   BC     = (Asymptotic) bias correction (i.e. degrees of freedom) for a 
#  #            (generalized) linear mixed model with binary response.
#  #  
#	y                   <- object@resp$y
#	signCor             <- - 2 * y + 1  ## Signum correction Eta(0)-Eta(1) vs Eta(1)-Eta(0)
#	mu                  <- object@resp$mu
#	eta                 <- qlogis(mu)
#	workingMatrix       <- matrix(rep(y, length(y)), ncol = length(y))
#	diag(workingMatrix) <- 1 - diag(workingMatrix)
#	workingEta          <- diag(apply(workingMatrix, 2, function(x) qlogis(refit(object, newresp = x)@resp$mu) - eta))
#	return(sum(mu * (1 - mu) * signCor * workingEta))
#}
#
