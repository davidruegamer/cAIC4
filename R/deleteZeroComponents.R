#' Delete random effect terms with zero variance
#' 
#' Is used in the \code{\link{cAIC}} function if \code{method = "steinian"} and
#' \code{family = "gaussian"}. The function deletes all random effects terms
#' from the call if corresponding variance parameter is estimated to zero and
#' updates the model in \code{\link[lme4]{merMod}}.
#' 
#' Uses the \code{cnms} slot of \code{m} and the relative covariance factors to
#' rewrite the random effects part of the formula, reduced by those parameters
#' that have an optimum on the boundary. This is necessary to obtain the true
#' conditional corrected Akaike information. For the theoretical justification
#' see Greven and Kneib (2010). The reduced model formula is then updated. The
#' function deleteZeroComponents is then called iteratively to check if in the
#' updated model there are relative covariance factors parameters on the
#' boundary.
#' 
#' @param m An object of class \code{\link[lme4]{merMod}} fitted by
#' \code{\link[lme4]{lmer}} of the lme4-package.
#' @return An updated object of class \code{\link[lme4]{merMod}}
#' @section WARNINGS : For models called via \code{gamm4} no automated update
#' is available. Instead a warning with terms to omit from the model is
#' returned.
#' @author Benjamin Saefken \email{bsaefke@@uni-goettingen.de} \& David
#' Ruegamer
#' @seealso \code{\link[lme4]{lme4-package}}, \code{\link[lme4]{lmer}},
#' \code{\link[lme4]{getME}}
#' @references Greven, S. and Kneib T. (2010) On the behaviour of marginal and
#' conditional AIC in linear mixed models. Biometrika 97(4), 773-789.
#' @keywords regression
#' @export
#' @examples
#' 
#' ## Currently no data with variance equal to zero...
#' b <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' 
#' deleteZeroComponents(b)
#' 
deleteZeroComponents <-
function(m) {
  # A function that deletes all random effects terms if corresponding variance
  # parameter is estimated to zero.
  #
  # Args: 
  #   m     = Object of class lmerMod. Obtained by lmer()
  #
  # Returns:
  #   m/newMod = A model without zero estimated variance component
  #
  theta      <- getME(m, "theta")
  thetazero  <- which(theta == 0)
  
  if (length(thetazero) == 0) {  # every thing's fine
    return(m)
  }
    
  if (length(theta) == length(thetazero)) {  #  only lm left
    warning("Model has no random effects variance components larger than zero.")
    return(lm(nobars(formula(m)), model.frame(m)))
  }

  varBlockMatrices <- getME(m, "ST")
  cnms <- m@cnms
  
  if(exists("gamm4", m@optinfo)) {  # for gamm4 what to exclude from the model
    for(i in 1:length(varBlockMatrices)){
      if(any(diag(varBlockMatrices[[i]]) == 0)) {
         termWithZero <- cnms[[i]][which(diag(varBlockMatrices[[i]]) == 0)]
         cat("The term", ifelse(termWithZero=="(Intercept)",names(termWithZero),termWithZero[[1]]), 
          "has zero variance components. \n")
      }
    }
    stop("After removing the terms with zero variance components and refitting 
          the model cAIC can be called again.", call. = FALSE)
  }

  # if(is.null(m@optinfo$conv$lme4$code) || 
  #    m@optinfo$conv$lme4$code == -1) {
    for(i in 1:length(varBlockMatrices)){
      cnms[[i]] <- cnms[[i]][which(diag(varBlockMatrices[[i]]) != 0)]
    }
  # } else {    # in case of convergence failures
  #   nc  <- vapply(cnms, length, 1L)
  #   thl <- split(theta, rep.int(seq_along(nc), (nc * (nc + 1))/2))
  #   for (i in 1:length(nc)) {
  #     ranVars   <- thl[[i]][1:nc[i]]
  #     cnms[[i]] <- cnms[[i]][which(ranVars != 0)] 
  #   }    
  # }
  
  reFormula  <- cnms2formula(cnms)
  if(suppressWarnings(nobars(formula(m)) == formula(m)[[2]])) {  # if there are no fixed effects 
    rhs      <- reFormula
  } else {
    rhs      <- c(attr(terms(nobars(formula(m))), "term.labels"), reFormula)
  }
  lhs        <- formula(m)[[2]]  # left hand side of the formula
  newFormula <- reformulate(rhs, lhs)  # merge both sides           
  newMod     <- update(m, formula. = newFormula, evaluate = TRUE)

  return(deleteZeroComponents(newMod))
}
