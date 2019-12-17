#' Prediction of model averaged linear mixed models
#'
#' Function to perform prediction for model averaged linear mixed models based
#' on the weight selection criterion as proposed by Zhang et al.(2014)
#'
#' @param object A object created by the model averaging function.
#' @param new.data Object that contains the data on which the prediction is to be based on.
#' @return An object that containing predictions that are calculated on the basis of dataset and the underlying averaged model.
#' @author Benjamin Saefken & Rene-Marcel Kruse
#' @seealso \code{\link[lme4]{lme4-package}}, \code{\link[lme4]{lmer}}
#' @references Greven, S. and Kneib T. (2010) On the behaviour of marginal and
#' conditional AIC in linear mixed models. Biometrika 97(4), 773-789.
#' @keywords model averaging, prediction, linear mixed models
#' @rdname predictMA
#' @export predictMA
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
#' predictMA(foo, new.data = Orthodont)
#' 
#'
predictMA <- function(object, new.data){
  z <- object
  c <- z$candidatmodels
  w <- z$optimresults$weights
  pmodels <- sapply(z$candidatmodels, predict, newdata = new.data)
  MApredict <- w%*%t(sapply(c, predict, newdata = new.data))
  res <- list(prediction = MApredict, weights = w)
  return(res)
}
