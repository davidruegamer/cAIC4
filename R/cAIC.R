#' Conditional Akaike Information for 'lme4' and 'lme'
#' 
#' Estimates the conditional Akaike information for models that were fitted in
#' 'lme4' or with 'lme'. Currently all distributions are supported for 'lme4' models, 
#' based on parametric conditional bootstrap. 
#' For the Gaussian distribution (from a \code{\link[lme4]{lmer}} or \code{\link[nlme]{lme}}
#' call) and the Poisson distribution analytical estimators for the degrees of
#' freedom are available, based on Stein type formulas. Also the conditional
#' Akaike information for generalized additive models based on a fit via the
#' 'gamm4' or \code{\link[mgcv]{gamm}} calls from the 'mgcv' package can be estimated.
#' A hands-on tutorial for the package can be found at \url{https://arxiv.org/abs/1803.05664}.
#' 
#' @param object An object of class merMod either fitted by
#' \code{\link[lme4]{lmer}} or \code{\link[lme4]{glmer}} of the lme4-package
#' or an \code{\link[nlme]{lme}} object fro the nlme-package.
#' Also objects returned form a \code{\link[gamm4]{gamm4}} call are possible.
#' @param method Either \code{"conditionalBootstrap"} for the estimation of the
#' degrees of freedom with the help of conditional Bootstrap or
#' \code{"steinian"} for analytical representations based on Stein type
#' formulas. The default is \code{NULL}. In this case the method is choosen
#' automatically based on the \code{family} argument of the
#' \code{(g)lmer}-object. For \code{"gaussian"} and \code{"poisson"} this is
#' the Steinian type estimator, for all others it is the conditional Bootstrap.
#' For models from the nlme package, only \code{\link[nlme]{lme}} objects, i.e.,
#' with gaussian response are supported.
#' @param B Number of Bootstrap replications. The default is \code{NULL}. Then
#' B is the minimum of 100 and the length of the response vector.
#' @param sigma.penalty An integer value for additional penalization in the analytic 
#' Gaussian calculation to account for estimated variance components in the residual (co-)variance. 
#' Per default \code{sigma.penalty} is equal \code{1}, corresponding to a diagonal error 
#' covariance matrix with only one estimated parameter (sigma). If 
#' all variance components are known, the value should be set to \code{0}. 
#' For individual weights (individual variances), this value should be set
#' to the number of estimated weights. For \code{\link[nlme]{lme}} objects
#' the penalty term is automatically set by extracting the number of estimated
#' variance components.
#' @param analytic FALSE if the numeric hessian of the (restricted) marginal
#' log-likelihood from the lmer optimization procedure should be used.
#' Otherwise (default) TRUE, i.e.  use a analytical version that has to be
#' computed. Only used for the analytical version of Gaussian responses.
#' @return A \code{cAIC} object, which is a list consisting of: 
#' 1. the conditional log likelihood, i.e. the log likelihood with the random 
#' effects as penalized parameters; 2. the estimated degrees of freedom; 
#' 3. a list element that is either \code{NULL}
#' if no new model was fitted otherwise the new (reduced) model, see details;
#' 4. a boolean variable indicating whether a new model was fitted or not; 5.
#' the estimator of the conditional Akaike information, i.e. minus twice the
#' log likelihood plus twice the degrees of freedom.
#' @section WARNINGS : Currently the cAIC can only be estimated for
#' \code{family} equal to \code{"gaussian"}, \code{"poisson"} and
#' \code{"binomial"}. Neither negative binomial nor gamma distributed responses
#' are available. 
#' Weighted Gaussian models are not yet implemented.
#' 
#' @details 
#' For \code{method = "steinian"} and an object of class \code{merMod} computed
#' the analytic representation of the corrected conditional AIC in Greven and
#' Kneib (2010). This is based on a the Stein formula and uses implicit
#' differentiation to calculate the derivative of the random effects covariance
#' parameters w.r.t.  the data. The code is adapted form the one provided in
#' the supplementary material of the paper by Greven and Kneib (2010). The
#' supplied \code{\link[lme4]{merMod}} model needs to be checked if a random
#' effects covariance parameter has an optimum on the boundary, i.e. is zero.
#' And if so the model needs to be refitted with the according random effect
#' terms omitted. This is also done by the function and the refitted model is
#' also returned. Notice that the \code{boundary.tol} argument in
#' \code{\link[lme4]{lmerControl}} has an impact on whether a parameter is
#' estimated to lie on the boundary of the parameter space. For estimated error
#' variance the degrees of freedom are increased by one per default. 
#' \code{sigma.penalty} can be set manually for \code{\link[lme4]{merMod}} models 
#' if no (0) or more than one variance component (>1) has been estimated. For 
#' \code{\link[nlme]{lme}} objects this value is automatically defined.
#' 
#' If the object is of class \code{\link[lme4]{merMod}} and has \code{family =
#' "poisson"} there is also an analytic representation of the conditional AIC
#' based on the Chen-Stein formula, see for instance Saefken et. al (2014). For
#' the calculation the model needs to be refitted for each observed response
#' variable minus the number of response variables that are exactly zero. The
#' calculation therefore takes longer then for models with Gaussian responses.
#' Due to the speed and stability of 'lme4' this is still possible, also for
#' larger datasets.
#' 
#' If the model has Bernoulli distributed responses and \code{method =
#' "steinian"}, \code{\link{cAIC}} calculates the degrees of freedom based on a
#' proposed estimator by Efron (2004). This estimator is asymptotically
#' unbiased if the estimated conditional mean is consistent. The calculation
#' needs as many model refits as there are data points.
#' 
#' Another more general method for the estimation of the degrees of freedom is
#' the conditional bootstrap. This is proposed in Efron (2004). For the B
#' boostrap samples the degrees of freedom are estimated by \deqn{\frac{1}{B -
#' 1}\sum_{i=1}^n\theta_i(z_i)(z_i-\bar{z}),} where \eqn{\theta_i(z_i)} is the
#' i-th element of the estimated natural parameter.
#' 
#' For models with no random effects, i.e. (g)lms, the \code{\link{cAIC}}
#' function returns the AIC of the model with scale parameter estimated by REML.
#' 
#' @author Benjamin Saefken, David Ruegamer
#' @seealso \code{\link[lme4]{lme4-package}}, \code{\link[lme4]{lmer}},
#' \code{\link[lme4]{glmer}}
#' @references 
#' Saefken, B., Ruegamer, D., Kneib, T. and Greven, S. (2018):
#' Conditional Model Selection in Mixed-Effects Models with cAIC4.
#' \url{https://arxiv.org/abs/1803.05664}
#' 
#' Saefken, B., Kneib T., van Waveren C.-S. and Greven, S. (2014) A
#' unifying approach to the estimation of the conditional Akaike information in
#' generalized linear mixed models. Electronic Journal Statistics Vol. 8,
#' 201-225.
#' 
#' Greven, S. and Kneib T. (2010) On the behaviour of marginal and conditional
#' AIC in linear mixed models. Biometrika 97(4), 773-789.
#' 
#' Efron , B. (2004) The estimation of prediction error. J. Amer. Statist. Ass.
#' 99(467), 619-632.
#' @keywords regression
#' @export
#' @import lme4 Matrix methods RLRsim mvtnorm
#' @importFrom stats terms.formula
#' @importFrom stats gaussian printCoefmat residuals
#' @importFrom utils capture.output
#' @rawNamespace 
#' if(getRversion() >= "3.3.0") {
#' importFrom("stats", sigma)
#' } else {
#' importFrom("lme4", sigma)
#' }
#' @examples
#' 
#' ### Three application examples
#' b <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' cAIC(b)
#' 
#' b2 <- lmer(Reaction ~ (1 | Days) + (1 | Subject), sleepstudy)
#' cAIC(b2)
#' 
#' b2ML <- lmer(Reaction ~ (1 + Days | Subject), sleepstudy, REML = FALSE)
#' cAIC(b2ML)
#' 
#' ### Demonstration of boundary case
#' \dontrun{
#' set.seed(2017-1-1)
#' n <- 50
#' beta <- 2
#' x <- rnorm(n)
#' eta <- x*beta
#' id <- gl(5,10)
#' epsvar <- 1
#' data <- data.frame(x = x, id = id)
#' y_wo_bi <- eta + rnorm(n, 0, sd = epsvar) 
#' 
#' # use a very small RE variance
#' ranvar <- 0.05
#' nrExperiments <- 100
#' 
#' sim <- sapply(1:nrExperiments, function(j){
#' 
#' b_i <- scale(rnorm(5, 0, ranvar), scale = FALSE)
#' y <- y_wo_bi + model.matrix(~ -1 + id) %*% b_i
#' data$y <- y
#' 
#' mixedmod <- lmer(y ~ x + (1 | id), data = data)
#' linmod <- lm(y ~ x, data = data)
#' 
#' c(cAIC(mixedmod)$caic, cAIC(linmod)$caic)
#' })
#' 
#' rownames(sim) <- c("mixed model", "linear model")
#' 
#' boxplot(t(sim))
#' 
#' 
#' }
#' 
#' 
#' 
cAIC <-
function(object, method = NULL, B = NULL, sigma.penalty = 1, analytic = TRUE) {
  
  if (any(names(object) == "mer")) {
    object <- object$mer
    object@optinfo$gamm4 <- TRUE    # add indicator for gamm4
  }
  
  if (any(class(object) %in% "gamm")) {
    attr(object$lme, "smooth_names") <- get_names(object) # old names
    attr(object$lme, "is_gamm") <- TRUE # add indicator for mgcv::gamm
    attr(object$lme, "gam_form") <- formula(object$gam) # for refit
    object <- object$lme
    attr(object, "ordered_smooth") <- sort_sterms(object) # names as in gamm4
  }

  if (any(class(object) %in% "lme")) {
    sigma.penalty <- count_par(object)
    if (!any(object$dims$ngrps > 1)) stop("No grouping structure specified.")
    if (is.null(attr(object, "is_gamm"))) attr(object, "is_gamm") <- FALSE
  }
  
  ### START: calculation for GLMs and LMs
  if (any(class(object) %in% c("glm","lm")) & !("gam" %in% class(object))) {
    
    y <- object$y

    if(is.null(y)) y <- eval(object$call$data, 
                             environment(formula(object)))[
                               all.vars(formula(object))[1]][[1]]
    if(is.null(y)) stop("Please specify the data argument in the initial model call!")
        
    n <- length(y)
    
    mu <- predict(object,type="response")
    p <- object$rank
    sigma <- ifelse("glm" %in% class(object),
                    sqrt(summary(object)$dispersion),
                    summary(object)$sigma * sqrt((n-p) / n))  
    
    switch(family(object)$family, binomial = {
      cll <- sum(dbinom(x = y, size = length(unique(y)) - 1, prob = mu, log = TRUE))
    }, poisson = {
      cll <- sum(dpois(x = y, lambda = mu, log = TRUE))
    }, gaussian = {
      cll <- sum(dnorm(x = y, mean = mu, sd = sigma, log = TRUE))
    }, {
      cat("For this family no bias correction is currently available \n")
      cll <- NA
    })
    
    retobj <-  list(loglikelihood = as.numeric(cll), 
                df            = object$rank + 1, 
                reducedModel  = NA, 
                new           = FALSE, 
                caic          = -2 * as.numeric(cll) + 2 * (object$rank + 1))
    class(retobj) <- c("cAIC")
    return(retobj)
  }
  ### END: calculation for GLMs and LMs
  
  if (!inherits(object, c("lmerMod", "glmerMod", "lme"))) {
    stop("Class of object is not supported.")
  }
  
  if (class(object) != "lme" && 
      family(object)$family == "binomial" && 
      length(unique(getME(object, "y"))) > 2) {
    warning("Method not yet supplied for binomial data with n larger 2. 
            Therefore the conditional parametric bootstrap is returned")
    method <- "conditionalBootstrap"
  }
  
  dfList   <- bcMer(object , 
                    method = method, 
                    B = B, 
                    sigma.penalty = sigma.penalty,
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
  
  if(new) cll <- getcondLL(newModel) else
    cll  <- getcondLL(object)
  caic <- - 2 * cll + 2 * bc
  retobj <- list(loglikelihood = cll, 
                 df            = bc, 
                 reducedModel  = newModel, 
                 new           = new, 
                 caic          = caic)
  class(retobj) <- c("cAIC")
  return(retobj)
}
