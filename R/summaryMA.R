#' Summary of model averaged linear mixed models
#'
#' Function to generate a summary of the results of the model averaging process.
#'
#' @param object A object created by the model averaging function.
#' @param randeff logical. Indicator whether the model averaged random effects should also be part of the output. The default setting is FALSE.
#' @return Outputs a summary of the model averaged random and fixed effects, as well as the calculated weights of the individual candidate models.
#' @author Benjamin Saefken & Rene-Marcel Kruse
#' @seealso \code{\link[lme4]{lme4-package}}, \code{\link[lme4]{lmer}}
#' @references Greven, S. and Kneib T. (2010) On the behaviour of marginal and
#' conditional AIC in linear mixed models. Biometrika 97(4), 773-789.
#' @keywords model averaging, linear mixed models
#' @rdname summaryMA
#' @export summaryMA
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
#' summaryMA(foo)
#'
#'
summaryMA <- function(object, randeff = FALSE){
  z <- object
  c <- z$call
  f <- z$fixeff
  r <- data.frame("group-specific" = z$raneff)
  o <- z$optimresults
  m <- z$candidatmodels

  cat("\nCall:\n",
      paste(deparse(c), sep="\n", collapse = "\n"), "\n\n", sep = "")

  cat("\nModel Averaged Fixed Effects:\n")
  print(f)

  if (randeff == TRUE) {
    cat("\nModel Averaged Fixed Effects:\n")
    printCoefmat(r)
  }

  cat("\nWeights for underlying Candidate Models:\n")
  print(round(o$weights, digits = 6))
}
