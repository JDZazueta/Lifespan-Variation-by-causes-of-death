# Title: LV Functions by Causes of Death
# Autor: Daniel


# ----------------------- e0 ---------------------------
LE_B <- function (Age, ex) {
  e0 <- ex[Age==0]
  return(e0)
}


# ----------------------- Gini ---------------------------
Gini.fun <- function (x, nax, ndx, ex) {
  e = rep(1, length(x))
  D = outer(ndx, ndx)
  x_ = x+nax
  X_ = abs(e%*%t(x_) - x_%*%t(e))
  G = sum(D*X_)/(2*ex[1L])
  return(g=G)
}

# ------------------------  Edag ----------------------------
Edag.fun <- function(dx, ex, lx){
  Edag <- rev(cumsum(rev(dx*ex)))/lx
  return(Edag)
}

# -----------------------  Edag JM  --------------------------
e.dagger.LT_CoD <- function(dx,ex,ax=ax, Pr){
  l <- length(ex)
  weight <-  dx/sum(dx)
  wd <- weight * Pr
  v <- (sum(wd[-l]* (ex[-l] + ax[-l]*(ex[-1]-ex[-l]) )) + ex[l])
  return(v)         
}

# --------------- IQR Funciton Alysons -----------------------
intFUN <- function(x, y, n, n.grid=10000){ 
  fit <- spline(x=x, y=y, n=n.grid)
  xi <- fit$x
  yi <- fit$y
  xn <- xi[which.min(abs(yi-n))]
  return(xn) 
}

# -----------------------  Mean Log  --------------------------
meanlog_CoD <- function(Age, dx, ex, Pr){
  weight <-  dx/sum(dx)
  weight_c <- weight * Pr
  e0 <- ex[1]
  log    <- ifelse(Age==0, 0, log(e0) - log(Age))
  mlog     <- weight_c* log
  mlog_all <- sum(mlog)
  return(mlog_all)
}

# ------------------------  Theil  ------------------------------
Theil_CoD <- function(Age, dx,ex, Pr){
  weight <-  dx/sum(dx)
  weight_c <- weight * Pr
  e0     <- ex[1]
  parte1    <- ifelse(Age==0, 0, Age/e0 * log(Age/e0))
  Theil <- weight_c * parte1
  Theil_all <- sum(Theil)
  return(Theil_all)
}

# -----------------------  Variance  --------------------------

LS_variance_CoD <- function(Age, dx, ex, Pr){
  weight <-  dx/sum(dx)
  weight_c <- weight * Pr
  e0     <- ex[1]
  dif    <-  (Age - e0)^2
  Var    <- weight_c*dif
  SD <- sqrt(Var)
  Var_total <- sum(Var)
  SD_total <- sqrt(Var_total)
  return(Var_total)
}

# -----------------------  Variance  --------------------------

SD_SA_CoD <- function(Age, dx, ex, Pr){
  weight <-  dx/sum(dx)
  weight_c <- weight * Pr
  e0     <- ex[1]
  dif    <-  (Age - e0)^2
  Var    <- weight_c*dif
  SD <- sqrt(Var)
  Var_total <- sum(Var)
  SD_total <- sqrt(Var_total)
  return(SD_total)
}

