#' Model Averaging for Linear Mixed Models
#'
#' Function to perform Model Averaging for Linear Mixed Models based
#' on the weight selection criterion Model Averaging as proposed by Zhang et al.(2014)
#'
#' @param models A list object containing all considered candidate models fitted by
#' \code{\link[lme4]{lmer}} of the lme4-package or of class
#' \code{\link[nlme]{lme}}.
#' @param opt logical. If TRUE (the default) the model averaging approach
#' with optimial weights is calculated. If FALSE the underlying weights as smoothed weights
#' as proposed by Buckland et al. (1997)
#' @return An object containing the function calls of the underlying candidate models,
#' the values of the model averaged fixed effects, the values of the model averaged random effects
#' the results of the weight optimization process, as well as a list of the candidate models themselvs
#' @author Benjamin Saefken & Rene-Marcel Kruse
#' @seealso \code{\link[lme4]{lme4-package}}, \code{\link[lme4]{lmer}}
#' @references Greven, S. and Kneib T. (2010) On the behaviour of marginal and
#' conditional AIC in linear mixed models. Biometrika 97(4), 773-789.
#' @references Zhang, X., Zou, G., & Liang, H. (2014). Model averaging and
#' weight choice in linear mixed-effects models. Biometrika, 101(1), 205-218.
#' @keywords model averaging, weights, linear mixed models
#' @rdname modelAvg
#' @export modelAvg
#' @examples 
#' data(Orthodont, package = "nlme")
#' models <- list(
#'     model1 <- lmer(formula = distance ~ age + Sex + (1 | Subject) + age:Sex,
#'                data = Orthodont),
#'     model2 <- lmer(formula = distance ~ age + Sex + (1 | Subject),
#'                data = Orthodont),
#'     model3 <- lmer(formula = distance ~ age + (1 | Subject),
#'                  data = Orthodont),
#'     model4 <- lmer(formula = distance ~ Sex + (1 | Subject),
#'                 data = Orthodont))
#' foo <- modelAvg(models = models)
#' foo
#'
#'
modelAvg <- function(models, opt = TRUE){
  call <- match.call()
  if (opt == TRUE) {
    tempres <- getWeights(models)
  } else {
    invisible(capture.output(tempres <-anocAIC(models)))
    tempres$delta<- tempres$cAIC - min(tempres$cAIC)
    tempres$weights <- exp(-tempres$delta / 2) / sum(exp(-tempres$delta / 2))
  }
  # calculation model averaged coefficients
  betas <- list()
  for (i in 1:length(models)) {
    betas[[i]] <- getME(models[[i]], "fixef")
  }
  avg.betas <- list()
  for (i in 1:length(models)) {
    avg.betas[[i]] <- betas[[i]]*tempres$weights[i]
  }
  sum.avg.betas <- tapply((unlist(avg.betas)),
                          names(unlist(avg.betas)), FUN = sum)

  # random effects
  rand <- list()
  for (i in 1:length(models)) {
    rand[[i]] <- ranef(models[[i]])
  }
  avg.rand <- list()
  for (i in 1:length(models)) {
    dummy <- unlist(rand[[i]])
    avg.rand[[i]] <- dummy*tempres$weight[i]
  }
  sum.avg.rand <- tapply((unlist(avg.rand)),
                         names(unlist(avg.rand)), FUN = sum)


  res <- list(call = call,
              fixeff = sum.avg.betas,
              raneff = sum.avg.rand,
              optimresults = tempres,
              candidatmodels = models)
}

