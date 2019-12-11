summary.MA <- function(object, randeff = FALSE, ...){
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

