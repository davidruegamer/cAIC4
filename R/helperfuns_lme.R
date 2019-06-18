# family function for lme objects to have a generic function
# also working for lme models
family.lme <- function(object, ...) gaussian()
