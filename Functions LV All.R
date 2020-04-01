# Title: LV Functions for HMD data base
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

# -------------------- Standar Deviation --------------------
SD.fun <- function(Age, ax, ex, dx){
  SD <- sqrt(sum((dx/100000) * (Age + ax - ex)^2))
  return(SD)
}

# -----------------------  Edag JM  --------------------------
e.dagger.LT <- function(fx,ex,ax=ax){
  l <- length(ex)
  v <- (sum(fx[-l]* (ex[-l] + ax[-l]*(ex[-1]-ex[-l]) )) + ex[l])
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
meanlog <- function(Age, dx, ex){
  weight <- dx / sum(dx)
  e0 <- ex[1]
  log    <- ifelse(Age==0, 0, log(e0) - log(Age))
  mlog     <- weight* log
  mlog_all <- sum(mlog)
  return(mlog_all)
}

# ------------------------  Theil  ------------------------------
Theil <- function(Age, dx, ex){
  weight <- dx / sum(dx)
  e0     <- ex[1]
  parte1    <- ifelse(Age==0, 0, Age/e0 * log(Age/e0))
  Theil <- weight * parte1
  Theil_all <- sum(Theil)
  return(Theil_all)
}

# -----------------------  Variance  --------------------------

LS_variance <- function(Age, dx, ex){
  N      <- sum(dx)
  weight <- dx / sum(dx)
  e0     <- ex[1]
  dif    <-  (Age - e0)^2
  Var    <- weight*dif
  SD <- sqrt(Var)
  Var_total <- sum(Var)
  SD_total <- sqrt(Var_total)
  return(Var_total)
}

# -----------------------  Variance  --------------------------

SD_SA <- function(Age, dx, ex){
  N      <- sum(dx)
  weight <- dx / sum(dx)
  e0     <- ex[1]
  dif    <-  (Age - e0)^2
  Var    <- weight*dif
  SD <- sqrt(Var)
  Var_total <- sum(Var)
  SD_total <- sqrt(Var_total)
  return(SD_total)
}


