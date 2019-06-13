biasCorrectionGaussian <-
function(m, sigma.penalty, analytic) {
  # A function that calls the bias correction functions.
  #
  # Args: 
  #   mer    = Object of class lmerMod or lme
  #   sigma.penalty = Number of estimated variance components in the residual error covariance
  #   analytic = FALSE if the numeric hessian of the (restricted) marginal log-
  #              likelihood from the lmer optimization procedure should be used.
  #              Otherwise (default) TRUE, i.e. use a analytical version that 
  #              has to be computed.
  #
  # Returns:
  #   bc = Bias correction for a mixed model.
  #
  zeroLessModel <- deleteZeroComponents(m)
  if (inherits(zeroLessModel, "lm")) {
    return(zeroLessModel$rank)
  }
  model <- getModelComponents(zeroLessModel, analytic)
  if (identical(m, zeroLessModel)) {
    bc       <- calculateGaussianBc(model, sigma.penalty, analytic)
    newModel <- NULL
    new      <- FALSE
  } else {
    bc       <- calculateGaussianBc(model, sigma.penalty, analytic)
    newModel <- zeroLessModel
    new      <- TRUE
  }
  return(list(bc = bc, newModel = newModel, new = new))
}
