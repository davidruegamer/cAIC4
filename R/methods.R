#' Print method for cAIC
#' @method print cAIC
#' @param x a cAIC object
#' @param digits number of digits to print
#' @param ... further arguments passed to 
#' generic print function (not in use).
#' @export
print.cAIC <- function(x, ..., digits = 2){
  
  prdf <- data.frame(
    a = c("Conditional log-likelihood: ",
          "Degrees of freedom: ",
          "Conditional Akaike information criterion: "),
    b = round(unlist(x[c("loglikelihood", "df", "caic")]), digits = digits))
  colnames(prdf) <- c()
  
  if(x$new){
    cat("The original model was refitted due to zero variance components.\n")
    cat("Refitted model: ", Reduce(paste, deparse(formula(x$reducedModel))), "\n")
  }
    
  print(prdf, row.names = FALSE)
  invisible(prdf)
  
}


#' Comparison of several lmer objects via cAIC
#' 
#' Takes one or more \code{lmer}-objects and produces a table 
#' to the console.
#' 
#' @param object a fitted \code{lme4}-object
#' @param ... additional objects of the same type
#' @param digits number of digits to print
#' 
#' @seealso \code{\link{cAIC}} for the model fit.
#' 
#' @return a table comparing the cAIC relevant information of all models
#' 
#' @export
anocAIC <- function(object, ..., digits = 2) {
  
  # get list of models
  objs <- c(object, list(...))
  
  # check correct input
  if(any(sapply(objs, function(x) !inherits(x, "merMod"))))
    stop("anocAIC can only deal with objects of class lmerMod or glmerMod")
  
  # calculate cAICs
  cAICs <- lapply(objs, cAIC)
  
  # extract formulas
  frms <- sapply(objs, function(x) Reduce(paste, deparse(formula(x))))
  # replace formulas, where the model was refitted
  refit <- sapply(cAICs, "[[", "new")
  if(any(refit))
    frms[which(refit)] <- sapply(cAICs[which(refit)], function(x) 
      Reduce(paste, deparse(formula(x$reducedModel))))
  
  # create returning data.frame
  ret <- as.data.frame(do.call("rbind", lapply(cAICs, function(x)  
    round(unlist(x[c("loglikelihood", "df", "caic", "new")]), digits = digits))))
  ret[,4] <- as.logical(ret[,4])
  rownames(ret) <- make.unique(frms, sep = " % duplicate #")
  colnames(ret) <- c("cll",
                     "df",
                     "cAIC",
                     "Refit")
  
  # print and return
  print(ret)
  invisible(ret)
}
