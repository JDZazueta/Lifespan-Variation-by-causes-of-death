# Title: Functions of inequality
# Autor: Daniel



#  KLD  -- From Issac Sasson
DecompKLD <- function(d1, d2) {
  cmean <- (mean(d1) - mean(d2))^2 / (2*var(d2))
  cvar <- log(sd(d2) / sd(d1)) + var(d1) / (2*var(d2)) - .5
  KLD <- cmean + cvar
  res <- data.frame(Mean=cmean, Var=cvar, Total=KLD,
                    pMean=round(100 * cmean / KLD, 1), 
                    pVar=round(100 * cvar / KLD, 1))
  return(res)
}

#-- Mean log deviation

meanlog <- function(Age, dx, ex){
  
  weight <- dx / sum(dx)
  
  # We need life expectancy at birth
  e0 <- ex[1]
  # Differneces between life expectancy at age x and life mean life expectancy
  
  log    <- ifelse(Age==0, 0, log(e0) - log(Age))
  
  mlog     <- weight* log
  
  mlog_all <- sum(mlog)
  
  meanlog_table <- data.frame(Age  = Age,
                              e0   = e0,
                              ex   = ex,
                              dx   = dx,
                              weight = weight,
                              mlog = mlog,
                              mlog_all = mlog_all
                              )
  return(mlog_all)
}

# -- Variance

LS_variance <- function(Age, dx, ex){
  
  # Total of deaths
  N      <- sum(dx)
  
  weight <- dx / sum(dx)
  
  # We need life expectancy at birth
  e0     <- ex[1]
  
  dif    <-  (Age - e0)^2
  #dif2   <-  dx*dif
  Var    <- weight*dif
  
  SD <- sqrt(Var)
  
  Var_total <- sum(Var)
  SD_total <- sqrt(Var_total)
  LVvar_table <- data.frame(Age  = Age,
                            N  = N,
                              e0   = e0,
                              ex   = ex,
                              dx   = dx,
                              var = Var,
                            SD = SD,
                            Var_total = Var_total,
                            SD_total = SD_total
  )
  return(LVvar_table)
}


## - IQR
intFUN <- function(x, y, n, n.grid=10000){ 
  fit <- spline(x=x, y=y, n=n.grid)
  xi <- fit$x
  yi <- fit$y
  xn <- xi[which.min(abs(yi-n))]
  return(xn) 
}

# ---- Theil

Theil <- function(Age, dx, ex){
  
  weight <- dx / sum(dx)
  
  # We need life expectancy at birth
  e0     <- ex[1]
  #la     <- 1/100000
  
  parte1    <- ifelse(Age==0, 0, Age/e0 * log(Age/e0))

  Theil <- weight * parte1
  
  Theil_all <- sum(Theil)
  
  theil_table <- data.frame(Age  = Age,
                            e0   = e0,
                            ex   = ex,
                            dx   = dx,
                            weight = weight,
                            Theil = Theil,
                            Theil_all = Theil_all
  )
  return(theil_table)
}






