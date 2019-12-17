# family function for lme objects to have a generic function
# also working for lme models
family.lme <- function(object, ...) gaussian()

sort_sterms <- function(m) {

  ## takes an nlme::lme object, orders and renames the smooth parts of the
  ## lme$data part as they are ordered in gamm4()

  # gamm() sorts the s()-terms as they appear in the model formula.
  # gamm4() respects ordering of s()-terms by, first, the s()-term with the
  # highest k comes first, in case of equal k, the s()-term are ordered as they
  # appear in the model formula with the latter terms in the model formula
  # appearing former in the columns of Z. Disregarding the order would lead to
  # false matrix multiplication.

  # only the names attribute is left after sorting

  sterm_index <- grep("^Xr", names(m$data))
  if(length(sterm_index) == 0) return(NULL)
  old_names <- names(m$data)[sterm_index]
  smooth_names <- attr(m, "smooth_names")
  names(m$data)[sterm_index] <- paste0("s.", smooth_names)
  knots_p_sterm <- sapply(m$data[sterm_index], ncol)

  # append old ordering to names for tracking later on
  names(knots_p_sterm) <- paste(names(knots_p_sterm),
    1:length(knots_p_sterm),
    sep = ".."
  )

  # if two single smooth terms have equal k
  uknots <- unique(knots_p_sterm)
  how_often_unique <- sapply(uknots, function(x) sum(knots_p_sterm %in% x))

  knots_p_sterm <- sort(knots_p_sterm, decreasing = TRUE)

  if (length(uknots) == 1) knots_p_sterm <- rev(knots_p_sterm)

  if (any(how_often_unique > 1)) { # if multiple smooth terms have equal k
    g <- uknots[how_often_unique > 1]
    for (ind in g) {
      index <- which(ind == knots_p_sterm)
      names(knots_p_sterm)[index] <- rev(names(knots_p_sterm)[index])
    }
  }
  n_knots <- names(knots_p_sterm)
  new_order <- as.numeric(substr(n_knots, nchar(n_knots), nchar(n_knots)))
  names(knots_p_sterm) <- substr(n_knots, 1, nchar(n_knots) - 3)
  attr(knots_p_sterm, "old_names") <- old_names[new_order]
  knots_p_sterm
}

get_names <- function(x) {

  # extracts the names of the smooth parts of an mgcv::gamm object and returns
  # them as they appear in the lme$data part of the original gamm$object. The
  # order of the terms is crucial later on owing to the different ordering of
  # smooth terms in gamm and gamm4. See sort_sterms for details.

  unlist(sapply(x$gam$smooth, function(x) {
    if (is.list(x[[1]])) {
      return(paste0(x$label, collapse = "_"))
    } # interaction te()
    x$label # regular s()
  }))
}


get_ST <- function(m) {

  # extracts from a fitted nlme::lme object and returns a list equivalent to
  # what is returned by getME(mer, "ST")

  theta <- get_theta(m)
  cnms <- attr(m$modelStruct$reStruct[[1]], "Dimnames")[1] # equivalent to lme4
  cnms <- c(cnms, attr(m, "smooth_names")) # relevant for mgcv::gamm

  cnms <- cor_re(m, cnms) # if random itcpt and slope are dependent cnms changes
  no_re <- lengths(cnms)

  vec2mlist(theta, no_re)
}

# split vector at position and return a list with the splitted elements
split_at <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))

# vec2mlist, get_clen, vec2STlist and sdiag are functions taken from the lme4
# package

vec2mlist <- function(v, n = NULL, symm = FALSE) {
  n <- get_clen(v, n)
  s <- split(v, rep.int(seq_along(n), n * (n + 1) / 2))
  m <- mapply(function(x, n0) {
    m0 <- diag(nrow = n0)
    m0[lower.tri(m0, diag = TRUE)] <- x
    if (symm) {
      m0[upper.tri(m0)] <- t(m0)[upper.tri(m0)]
    }
    m0
  }, s, n, SIMPLIFY = FALSE)
  m
}

get_clen <- function(v, n = NULL) {
  if (is.null(n)) {
    if (is.null(n <- attr(v, "clen"))) {
      n <- (sqrt(8 * length(v) + 1) - 1) / 2
    }
  }
  n
}


vec2STlist <- function(v, n = NULL) {
  ch <- vec2mlist(v, n, FALSE)

  lapply(ch, function(L) {
    ST <- L %*% sdiag(1 / sdiag(L))
    diag(ST) <- diag(L)
    ST
  })
}

sdiag <- function(x) {
  if (length(x) == 1) {
    matrix(x, 1, 1)
  } else {
    diag(x)
  }
}

cor_re <- function(m, cnms) {

  # splits the first entry of cnms into two parts if random itcpt and slope
  # were modelled dependently and returns the splitted version if so

  if (!is_dep(m) & lengths(cnms[1]) == 2) {
    # uncorrelated re have different cnms dim than correlated ones
    rev(c(split_at(cnms[[1]], 2), cnms[-1]))
  } else {
    cnms
  }
}

get_D <- function(m) {
  
  # extracts getME(mer, "Lambda) %*% getME(mer, "Lambdat") first as in lme4
  # and then returns the vCov of the random effects (D) in the order as they 
  # are returned in lme4 (D = getME(mer, "Lambda) %*% getME(mer, "Lambdat") *
  # sigma(mer) ^ 2). This needs special treatment with repect to the smooth
  # terms which appear in different order inside the reStruct list and need to
  # ordered first.
  
  D_lt <- lapply(m$modelStruct$reStruct, as.matrix)
  n <- m$dims$ngrps[1]
  no_knots <- sum(attr(m, "ordered_smooth"))
  
  # in case of smooth terms
  if (length(D_lt) > 1) {
    D_lt_2 <- D_lt
    re_formula <- lapply(m$modelStruct$reStruct[-1], function(x) formula(x))
    re_formula <- sapply(re_formula, function(x) as.character(x[[2]][[2]]))
    ordered_n <- attr(attr(m,"ordered_smooth"),"old_names")
    D_lt_2 <- D_lt_2[names(re_formula[match(ordered_n,re_formula)])]
  }
  
  D_lt <- lapply(1:n, function(x) D_lt[[1]]) # substitute for rep() with matrix
  D_lt <- bdiag(D_lt)
  
  # in case of independent random effects
  if (!is_dep(m) & m$dims$qvec[1] == 2) {
    D_lt <- diag(D_lt)
    fir <- D_lt[seq(1,length(D_lt),2)]
    sec <- D_lt[seq(1,length(D_lt),2) + 1]
    D_lt <- Matrix(diag(c(fir,sec)))
    if (!no_knots == 0) D_lt <-  Matrix(diag(c(sec,fir)))
  } 

  if (exists("D_lt_2")) return(bdiag(c(D_lt,bdiag(D_lt_2))) * sigma(m)^2)
  D_lt * sigma(m)^2
}

get_theta <- function(m) {

  # extracts equivalent to getME(mer,"theta") from a nlme::lme. For now, only
  # random intercept (+ slope, correlated and uncorrelated) can be handled.

  re_vcov <- nlme::VarCorr(m)
  sigma <- sigma(m)

  rnames <- rownames(re_vcov)
  idx1 <- grep("(Intercept)", rnames)
  idx2 <- grep("Residual", rnames)
  var_re_itcpt <- as.numeric(re_vcov[idx1, "Variance"])
  theta_1 <- matrix(sqrt(var_re_itcpt) / sigma)

  # more than a random intercept
  if (idx2 - idx1 > 1) {
    var_re_slope <- as.numeric(re_vcov[idx1 + 1, "Variance"])
    D <- matrix(c(var_re_itcpt, 0L, 0L, var_re_slope), ncol = 2, byrow = TRUE)
    theta_1 <- sqrt(diag(D)) / sigma
    if (is_dep(m)) {
      cov_re <- as.numeric(re_vcov[idx1 + 1, "Corr"]) * sqrt(var_re_itcpt *
        var_re_slope)
      D[2:3] <- cov_re
      theta_1 <- base::chol(D) / sigma
      theta_1 <- theta_1[upper.tri(theta_1, diag = TRUE)]
    }
  }

  if (!attr(m, "is_gamm") | length(grep("^g.", rnames)) == 0) {
    return(theta_1)
  }

  spline_var <- as.numeric(re_vcov[grep("^g.", rnames) + 1, 1])
  names(spline_var) <- attr(m, "smooth_names") # gamm order

  # these terms have gamm4 order
  ordered_sterms <- names(attr(m, "ordered_smooth"))
  ordered_sterms <- substr(ordered_sterms, 3, nchar(ordered_sterms))

  # terms are now ordered in line with columns of getME(mer,"Z")
  spline_var <- spline_var[ordered_sterms] # has gamm4 order
  theta_2 <- sqrt(spline_var) / sigma

  c(theta_1, theta_2)
}

count_par <- function(m, sigma.estimated = TRUE) {

  # takes a fitted nlme::lme and returns the number of
  # estimated paramters that were used to specify the residual matrix of a mixed
  # model. Per default, the residual variance is assumed unknown and thus
  # estimated.

  var_str <- as.vector(m$modelStruct[["varStruct"]])
  cor_str <- as.vector(m$modelStruct[["corStruct"]])

  length(var_str) + length(cor_str) + sigma.estimated
}

is_dep <- function(m) "Corr" %in% colnames(nlme::VarCorr(m))

get_Z <- function(m) {

  ## extracts equivalent to getME(mer,"Z") from a nlme::lme object.

  # the spline part of RLRsim::extract.lmeDesign$Z is not convenient for usage
  # since the ordering of the columns is not arbitrary and not in line with
  # getME(mer,"Z")

  Z_try <- RLRsim::extract.lmeDesign(m)$Z
  
  no_knots <- sum(attr(m, "ordered_smooth"))

  # get (true) random effect part from Z matrix
  random_part <- Z_try[, (no_knots + 1):ncol(Z_try)]
  
  if (!is_dep(m) & m$dims$qvec[1] == 2) {
    col <- seq(1,ncol(random_part),2)
    fir <- random_part[,col, drop = FALSE]
    sec <- random_part[,col + 1, drop = FALSE]
    random_part <- cbind(fir,sec)
    if (!no_knots == 0) random_part <- cbind(sec, fir)
  } 

  # get the spline part of Z order it according to getME(mer,"Z")
  spline_part <- m$data[attr(attr(m, "ordered_smooth"), "old_names")]

  Matrix(as.matrix(cbind(random_part, spline_part)))
}

get_LambdaT <- function(m) {

  # extracts equivalent to getME(mer,"Lambdat") from a nlme::lme object.

  D <- get_D(m)

  # relative covariance (divide by residual variance)
  rel_vcov <- D / (sigma(m)^2)
  # may consider Cholesky() instead: Cholesky(chol_prep, LDL = FALSE, Imult=1)
  list(LambdaT = Matrix(base::chol(rel_vcov)), D = D)
}

get_L <- function(m) {

  # extracts equivalent to getME(mer,"L") from a nlme::lme object

  # weights <- weights(m)
  # if(length(weights) == 0) weights <- rep(1,nrow(m$data))
  # sqrtW <- Diagonal(x = sqrt(as.numeric(weights)))
  # ZtW <- Zt %*% sqrtW
  Zt <- t(get_Z(m))
  R <- get_R(m)/(sigma(m)^2)

  ZtW <- Zt %*% chol(R)
  Lambdat <- get_LambdaT(m)
  as(
    Cholesky(tcrossprod(Lambdat %*% ZtW), LDL = FALSE, Imult = 1),
    "sparseMatrix"
  )
}

get_RX <- function(m) {

  # extracts equivalent to getME(mer,"RX") from a nlme::lme object

  chol(solve(m$varFix)) * sigma(m)
}

get_Lind <- function(m) {

  # extracts equivalent to getME(mer,"RX") from a nlme::lme object

  no_re <- m$dims$qvec[[1]] + (is_dep(m))
  l_re <- no_re <= 2
  n_groups <- m$dims$ngrps[[1]]
  knots_p_sterm <- attr(m, "ordered_smooth")
  i_g <- attr(m, "is_gamm")

  if ((!i_g & l_re) | (i_g & is.null(knots_p_sterm) & l_re)) {
    return(rep(1:no_re, each = n_groups))
  }
  if (!i_g | (i_g & is.null(knots_p_sterm))) {
    return(rep(1:no_re, n_groups))
  }

  term <- 1:length(knots_p_sterm) + no_re
  vemp <- vector("list", length(term))
  for (ind in seq_len(length(term))) {
    vemp[[ind]] <- rep(term[ind],knots_p_sterm[ind])
  }
  if (l_re) {
    return(c(rep(1:no_re, each = n_groups), unlist(vemp)))
  }
  c(rep(1:no_re, n_groups), unlist(vemp))
}

get_R <- function(m) {

  # returns the cond. VCov of a mixed model fitted with nlme::lme as a block -
  # diag matrix. In the case of homoscedastic error variance the main diagonal
  # contains sigma(m)^2. For now, a nested mixed model can not be handled.

  # some gamms contain a pesudo nesting structure (s-terms) which needs
  # to be exlcuded
  nlev_col <- sapply(m$groups, nlevels)
  if (ncol(m$groups) > 1) m$groups <- m$groups[, nlev_col > 1, drop = FALSE]

  n <- m$dims$ngrps[1]
  vcov_list <- sapply(1:n, function(x) nlme::getVarCov(m,
      type = "conditional", x
    ))
  Matrix::bdiag(vcov_list)
}

get_D_RLRSim <- function(m) {
  
  # extracts getME(mer, "Lambda) %*% getME(mer, "Lambdat") first as in lme4
  # and then returns the vCov of the random effects (D) in the order as they 
  # are returned in lme4 (D = getME(mer, "Lambda) %*% getME(mer, "Lambdat") *
  # sigma(mer) ^ 2). This needs special treatment with repect to the smooth
  # terms which appear in different order inside the reStruct list and need to
  # ordered first.
  
  res <- RLRsim::extract.lmeDesign(m)
  D <- res$Vr * res$sigmasq # inverse order of splines and re coefs

  # find vcov coef that belong to splines and those who belong to true re
  spline_index <- grep("^g", names(m$coefficients$random))
  no_knots <- length(unlist(m$coefficients$random[spline_index]))
  last_index <- nrow(D)

  if (length(spline_index) != 0) {
    # seperate parts
    spline_vcov <- D[1:no_knots, 1:no_knots]
    random_vcov <- D[(no_knots + 1):last_index, (no_knots + 1):last_index]

    # build D matrix as in gamm4() by just reordering D from RLRsim
    D <- matrix(0L, nrow = nrow(res$Vr), ncol = ncol(res$Vr)) # prepare
    D[1:(last_index - no_knots), 1:(last_index - no_knots)] <- random_vcov
    D[(last_index - no_knots + 1):last_index, (last_index - no_knots + 1):
    last_index] <- spline_vcov
  }
  
  D
}

