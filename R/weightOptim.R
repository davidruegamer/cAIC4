.weightOptim = function(weights, lm, targets, hess, lambda, scaler, .envi)
{
  m     <- get("m", envir = .envi)
  y     <- get("y", envir = .envi)
  mue   <- get("mu", envir = .envi)
  varDF <- get("varDF", envir = .envi)
  find_weights <- get("find_weights", envir = .envi)
  lowb  <- get("lowb", envir = .envi)
  uppb  <- get("uppb", envir = .envi)
  equB  <- get("equB", envir = .envi)
  rho   <- 0
  maxit <- 800
  delta <- (1.0e-7)
  tol   <- (1.0e-8)
  numw  <- length(m)
  ind   <- 1
  l     <- c(0,0,0)
  p0    <- weights
  ab    <- cbind(lowb,uppb)
  st    <- numeric()
  ptt   <- matrix()
  sc    <- numeric()
  
  # scale the cost, the equality constraints, the inequality constraints,
  # the parameters (inequality parameters AND actual parameters),
  targets <- targets / scaler[ 1:2 ]
  p0    <- p0 / scaler[ 3:(numw + 2) ]
  mm    <- numw
  ab    <- ab / cbind(scaler[ 3:(mm + 2) ], scaler[ 3:(mm + 2) ])
  # scale the lagrange multipliers and the Hessian
  lm    <- scaler[2] * lm / scaler[ 1 ]
  hess  <- hess * (scaler[ 3:(numw + 2) ] %*% t(scaler[ 3:(numw + 2)]) ) / scaler[ 1 ]
  j     <- targets[ 1 ]
  a     <- matrix(0, nrow = 1, ncol = numw)
  g     <- rep(0, times = length(m))
  p     <- p0 [ 1:numw ]
  constraint <- targets[ 2 ]
  # gradient:
  for( i in 1:numw ) {
    p0[ i ] <- p0[ i ] + delta
    tmpv    <- p0[ 1:numw ] * scaler[ 3:(numw + 2) ]
    funv 	  <- find_weights(tmpv)
    eqv     <- (sum(tmpv) - equB)
    targets <- c(funv, eqv) / scaler[ 1:2]
    g[ i ]  <- (targets[ 1 ] - j) / delta
    a[ , i ]<- (targets[ 2 ] - constraint) / delta
    p0[ i ] <- p0[ i ] - delta
  }
  b     <- a %*% p0 - constraint
  ind   <- -1
  l[1]  <- tol - max(abs(constraint))
  if( l[ 1 ] <= 0 ) {
    ind <- 1
    p0[ numw + 1 ] <- 1
    a   <- cbind(a, -constraint)
    cx  <- cbind(matrix(0, nrow = 1, ncol = numw), 1)
    dx  <- rep(1, times = length(m)+1)
    go  <- 1
    minit <- 0
    while( go >= tol ) {
      minit <- minit + 1
      gap   <- cbind(p0[ 1:mm ] - ab[ , 1 ], ab[ , 2 ] - p0[ 1:mm ] )
      gap   <- t( apply( gap, 1, FUN=function( x ) sort(x) ) )
      dx[ 1:mm ] <- gap[ , 1 ]
      dx[ numw + 1 ] <- p0[ numw + 1 ]
      y <- try( qr.solve( t( a %*% diag( as.numeric(dx) , length(dx), length(dx) ) ), dx * t(cx) ), silent = TRUE)
      if(inherits(y, "try-error")){
        p    <- p0 * scaler[ 3:(numw + 2) ]
        y    <- 0
        hess <- scaler[ 1 ] * hess / (scaler[ 3:(numw + 2) ] %*% t(scaler[ 3:(numw + 2) ]) )
        ans  <- list(p = p, y = y, hess = hess, lambda = lambda)
        return(ans)
      }
      v <- dx * ( dx *(t(cx) - t(a) %*% y) )
      if( v[ numw + 1 ] > 0 ) {
        z <- p0[ numw + 1 ] / v[ numw + 1 ]
        for( i in 1:mm ) {
          if( v[ i ] < 0 ) {
            z <- min(z, -(ab[ i, 2 ] - p0[ i ]) / v[ i ])
          } else if( v[ i ] > 0 ) {
            z <- min( z, (p0[ i ] - ab[ i , 1 ]) / v[ i ])
          }
        }
        if( z >= p0[ numw + 1 ] / v[ numw + 1 ] ) {
          p0 <- p0 - z * v
        } else {
          p0 <- p0 - 0.9 * z * v
        }
        go <- p0[ numw + 1 ]
        if( minit >= 10 ) {
          go <- 0
        }
      } else {
        go <- 0
        minit <- 10
      }
    }
    a <- matrix(a[ , 1:numw ], ncol = numw)
    b <- a %*% p0[ 1:numw ]
  }
  p <- p0 [ 1:numw ]
  y <- 0
  if( ind > 0 ) {
    tmpv <- p[ 1:numw ] * scaler[ 3:(numw + 2) ]
    funv <- find_weights(tmpv)
    eqv <- (sum(tmpv) - equB)
    targets <- c(funv, eqv) / scaler[ 1:2 ]
  }
  j <- targets[ 1 ]
  targets[ 2 ] <- targets[ 2 ] - a %*% p + b
  j <- targets[ 1 ] - t(lm) %*% matrix(targets[ 2 ], ncol=1) + rho * targets[ 2 ]^2
  minit <- 0
  while( minit < maxit ) {
    minit <- minit + 1
    if( ind > 0 ) {
      for( i in 1:numw ) {
        p[ i ] <- p[ i ] + delta
        tmpv <- p[ 1:numw ] * scaler[ 3:(numw + 2) ]
        funv 	<- find_weights(tmpv)
        eqv <- (sum(tmpv) - equB)
        targetsm <- c(funv, eqv) / scaler[ 1:2 ]
        targetsm[ 2 ] <- targetsm[ 2 ] - a %*% p + b
        targetsm <- targetsm[ 1 ] - t(lm) %*% targetsm[ 2 ] + rho * targetsm[ 2 ]^2
        g[ i ] <- (targetsm - j) / delta
        p[ i ] <- p[ i ] - delta
      }
    }
    if( minit > 1 ) {
      yg <- g - yg
      sx <- p - sx
      sc[ 1 ] <- t(sx) %*% hess %*% sx
      sc[ 2 ] <- t(sx) %*% yg
      if( (sc[ 1 ] * sc[ 2 ]) > 0 ) {
        sx <- hess %*% sx
        hess  <- hess - ( sx %*% t(sx) ) / sc[ 1 ] + ( yg %*% t(yg) ) / sc[ 2 ]
      }
    }
    dx <- matrix(rep(0.1, times = numw), nrow = numw, ncol = 1)
    gap <- cbind(p[ 1:mm ] - ab[ , 1 ], ab[ , 2 ] - p[ 1:mm ])
    gap <- t( apply( gap, 1, FUN = function( x ) sort(x) ) )
    gap <- gap[ , 1 ] + sqrt(.Machine$double.eps) * rep(1, times = mm)
    dx[ 1:mm, 1 ] <- rep(1, times = mm) / gap
    go <- -1
    lambda <- lambda / 10
    while( go <= 0 ) {
      cz <- try(chol( hess + lambda * diag( as.numeric(dx * dx), length(dx), length(dx) ) ),  silent = TRUE)
      if(inherits(cz, "try-error")){
        p <- p * scaler[ 3:(numw + 2) ]
        y <- 0
        hess <- scaler[ 1 ] * hess / (scaler[ 3:(numw + 2) ] %*% t(scaler[ 3:(numw + 2) ]) )
        ans <- list(p = p, y = y, hess = hess, lambda = lambda)
        return(ans)
      }
      cz <- try(solve(cz), silent = TRUE)
      if(inherits(cz, "try-error")){
        p <- p * scaler[ 3:(numw + 2) ]
        y <- 0
        hess <- scaler[ 1 ] * hess / (scaler[ 3:(numw + 2) ] %*% t(scaler[ 3:(numw + 2) ]) )
        ans <- list(p = p, y = y, hess = hess, lambda = lambda)
        return(ans)
      }
      yg <- t(cz) %*% g
      y <- try( qr.solve(t(cz) %*% t(a), yg), silent = TRUE )
      if(inherits(y, "try-error")){
        p <- p * scaler[ 3:(numw + 2) ]
        y <- 0
        hess <- scaler[ 1 ] * hess / (scaler[ 3:(numw + 2) ] %*% t(scaler[ 3:(numw + 2) ]) )
        ans <- list(p = p, y = y, hess = hess, lambda = lambda)
        return(ans)
      }
      u <- -cz %*% (yg - ( t(cz) %*% t(a) ) %*% y)
      p0 <- u[ 1:numw ] + p
      go <- min( c(p0[ 1:mm ] - ab[ , 1 ], ab[ , 2 ] - p0[ 1:mm ]) )
      lambda <- 3 * lambda
    }
    l[ 1 ] <- 0
    targets1 <- targets
    targets2 <- targets1
    st[ 1 ] <- j
    st[ 2 ] <- j
    ptt <- cbind(p, p)
    l[ 3 ] <- 1.0
    ptt <- cbind(ptt, p0)
    tmpv <- ptt[ 1:numw, 3 ] * scaler[ 3:(numw + 2) ]
    funv 	<- find_weights(tmpv)
    eqv <- (sum(tmpv) - equB)
    targets3 <- c(funv, eqv) / scaler[ 1:2 ]
    st[ 3 ] <- targets3[ 1 ]
    targets3[ 2 ] <- targets3[ 2 ] - a %*% ptt[ , 3 ] + b
    st[ 3 ] <- targets3[ 1 ] - t(lm) %*% targets3[ 2 ] + rho * targets3[ 2 ] ^ 2
    go <- 1
    while( go > tol ) {
      l[ 2 ] <- (l[ 1 ] + l[ 3 ]) / 2
      ptt[ , 2 ] <- (1 - l[ 2 ]) * p + l[ 2 ] * p0
      tmpv <- ptt[ 1:numw, 2 ] * scaler[ 3:(numw + 2) ]
      funv 	<- find_weights(tmpv)
      eqv <- (sum(tmpv) - equB)
      targets2 <- c(funv, eqv) / scaler[ 1:2 ]
      st[ 2 ] <- targets2[ 1 ]
      targets2[ 2 ] <- targets2[ 2 ] - a %*% ptt[ , 2 ] + b
      st[ 2 ] <- targets2[ 1 ] - t(lm) %*% targets2[ 2 ] + rho * targets2[ 2 ] ^ 2
      targetsm <- max(st)
      if( targetsm < j ) {
        targetsn <- min(st)
        go <- tol * (targetsm - targetsn) / (j - targetsm)
      }
      # Conditions:
      con1 <- st[ 2 ] >= st[ 1 ]
      con2 <- st[ 1 ] <= st[ 3 ] && st[ 2 ] < st[ 1 ]
      con3 <- st[ 2 ] <  st[ 1 ] && st[ 1 ] > st[ 3 ]
      if( con1 ) {
        st[ 3 ] <- st[ 2 ]
        targets3 <- targets2
        l[ 3 ] <- l[ 2 ]
        ptt[ , 3 ] <- ptt[ , 2 ]
      }
      if( con2 ) {
        st[ 3 ] <- st[ 2 ]
        targets3 <- targets2
        l[ 3 ] <- l[ 2 ]
        ptt[ , 3 ] <- ptt[ , 2 ]
      }
      if( con3 ) {
        st[ 1 ] <- st[ 2 ]
        targets1 <- targets2
        l[ 1 ] <- l[ 2 ]
        ptt[ , 1 ] <- ptt[ , 2 ]
      }
      if( go >= tol ) {
        go <- l[ 3 ] - l[ 1 ]
      }
    }
    sx <- p
    yg <- g
    ind <- 1
    targetsn <- min(st)
    if( j <= targetsn ) {
      maxit <- minit
    }
    reduce <- (j - targetsn) / ( 1 + abs(j) )
    if( reduce < tol ) {
      maxit <- minit
    }
    con1 <- st[ 1 ] <  st[ 2 ]
    con2 <- st[ 3 ] <  st[ 2 ] && st[ 1 ] >= st[ 2 ]
    con3 <- st[ 1 ] >= st[ 2 ] && st[ 3 ] >= st[ 2 ]
    if( con1 ) {
      j <- st[ 1 ]
      p <- ptt[ , 1 ]
      targets <- targets1
    }
    if( con2 ) {
      j <- st [ 3 ]
      p <- ptt[ , 3 ]
      targets <- targets3
    }
    if( con3 ) {
      j <- st[ 2 ]
      p <- ptt[ , 2 ]
      targets <- targets2
    }
  }
  p <- p * scaler[ 3:(numw + 2) ]
  y <- scaler[ 1 ] * y / scaler[ 2 ]
  hess <- scaler[ 1 ] * hess / (scaler[ 3:(numw + 2) ] %*% t(scaler[ 3:(numw + 2) ]) )
  ans <- list(p = p, y = y, hess = hess, lambda = lambda)
  return( ans )
}

