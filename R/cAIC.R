cAIC <-
function(object, method = NULL, B = NULL, sigma.estimated = TRUE, analytic = TRUE) {
  # A function that calls the bias correction functions.
  #
  # Args: 
  #   object = Object of class lmerMod or glmerMod. Obtained by lmer() or glmer()
  #   method = How the bias correction should be evaluated. If NULL than method 
  #            is chosen by family, i.e. analytical if family is Poisson or 
  #            Gaussian and with parametric bootstrap for other. Method may also
  #            be specified before, either "steinian" or "conditionalBootstrap".
  #            "steinian" only available for Gaussian, Poisson and Bernoulli.
  #   B      = Number of Bootstrap replications. Default is NULL then it is 
  #            chosen as maximum of the number of observations and 100.
  #   sigma.estimated = If sigma is estimated. This only is used for the 
  #                     analytical version of Gaussian responses.
  #   analytic = FALSE if the numeric hessian of the (restricted) marginal log-
  #              likelihood from the lmer optimization procedure should be used.
  #              Otherwise (default) TRUE, i.e. use a analytical version that 
  #              has to be computed.
  #
  # Returns:
  #   list   = The list contains the conditional log-likelihood; the estimated 
  #            conditional prediction error (degrees of freedom); If a new model
  #            was fitted, the new model and an boolean indicating weather a new
  #            model was fitted; the conditional Akaike information, caic.
  #
  if (any(names(object) == "mer")) {
    object <- object$mer
    object@optinfo$gamm4 <- TRUE    # add indicator for gamm4
  }
  
  if (class(object) %in% c("glm","lm")) {
    
    y <- object$y
    if(is.null(y)) y <- eval(object$call$data, environment(formula(object)))[all.vars(formula(object))[1]][[1]]
    if(is.null(y)) stop("Please specify the data argument in the initial model call!")
    
    mu <- predict(object,type="response")
    sigma <- ifelse("glm" %in% class(object), 
                    sqrt(summary(object)$dispersion), 
                    summary(object)$sigma)  
    
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
    
    return(list(loglikelihood = cll, 
                df            = 2 * length(object$coefficients), 
                reducedModel  = NA, 
                new           = NA, 
                caic          = -2 * cll + 2 * length(object$coefficients)))
  }
  
  if (!inherits(object, c("lmerMod", "glmerMod"))) {
    stop("Class of object is not known")
  }
  
  if (family(object)$family == "binomial" && length(unique(getME(object, "y"))) > 2) {
    warning("Method not yet supplied for binomial data with n larger 2. 
            Therefore the conditional parametric bootstrap is returned")
    method <- "conditionalBootstrap"
  }
  
  dfList   <- bcMer(object , 
                    method = method, 
                    B = B, 
                    sigma.estimated = sigma.estimated,
                    analytic = analytic)
  if (mode(dfList) == "list") {
    bc       <- dfList$bc
    newModel <- dfList$newModel
    new      <- dfList$new
  } else {
    bc       <- dfList
    newModel <- NULL
    new      <- FALSE
  }
  
  cll  <- getcondLL(object)
  caic <- - 2 * cll + 2 * bc
  return(list(loglikelihood = cll, 
              df            = bc, 
              reducedModel  = newModel, 
              new           = new, 
              caic          = caic))
}
