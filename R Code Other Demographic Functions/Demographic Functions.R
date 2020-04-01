# Title: Functions for the thesis
# Autor: Daniel


# ---------- Multiple Decrement life Table -----------------------

# The idea is to get a table exaclty like in Preston et al. (2001)

MDLT <- function(Age, lx, qx, pix){
  qix <- qx * pix
  dix <- qix * lx
  lix <- rev(cumsum(rev(dix)))
  results <- data.frame(Age = Age,
                        lx = lx,
                        qx = qx,
                        qix =  qix,
                        dix = dix,
                        lix = lix)
  return(results)
}

# ---------  Associated single decrement life table ---------------

ASDT <- function(Age, n, ax, qx, lx, dx, Lx, ex, pix){
  # where: ax, qx, lx, ex are from the HMD Life Table
  # The first aspect is to declare NA
  
  i.openage <- length(pix)
  OPENAGE   <- i.openage - 1
  
  # First we esttimate Ri in theory is (Dx-Dix)/Dx,
  # But we dont have the Dx so we take 1-(Dix/Dx)
  
  Ri <- 1-pix
  
  # Also we need the probability of sourviving
  px <- 1 - qx
  
  # Now we estimate the probability of soruviving by i cause
  pix <- (px)^Ri
  pix[is.na(pix)] <-0
  
  # Radix of life table
  lix <- rep(NA,length(pix))
  lix[1] <- 100000
  for(i in 1:length(n)){
    lix[i+1] <- lix[i]*pix[i]
  }
  lix <- lix[1:length(pix)]
  
  
  # Estimate the probability of dying by i cause
  qix <- 1- pix
  
  qix[i.openage]       <- ifelse(is.na(qix[i.openage]), NA, 1)

  # Now we estimatte the dx by i cause
  dix <- qix * lix
  
  # To estimate de Lx by i cause is 
  Lix <- rep(NA,length(lix))
  for(i in 1:OPENAGE){
    Lix[i] <- lix[i+1]*n[i] + dix[i]*ax[i]
  }
  Lix[length(n)] <- lix[length(n)] * ax[length(n)]
  
  Lix[i.openage ]	    <- lix[i.openage ] * ax[i.openage ]
  
  # To estimate Tx by i case
  Tix 	 <- c(rev(cumsum(rev(Lix[1:OPENAGE]))),0) + Lix[i.openage]
  
  # Life expectancy
  eix <- Tix/lix
  
  # Difference in life expectancy
  dle <- eix - ex
  
  results <- data.frame(Age = Age,
                        n   = n,
                        ax  = ax,
                        qx  = qx,
                        px  = px,
                        lx  = lx,
                        dx  = dx,
                        Lx  = Lx, 
                        ex  = ex,
                        pix = pix,
                        qix = qix,
                        dix = dix,
                        lix = lix,
                        Lix = Lix,
                        Tix = Tix,
                        eix = eix,
                        dle = dle)
  return(results)
}

# -- Function to fit Gompertz Law of Mortality to Estimate survivors at Older Ages
# -- Methodology based on Preston et al (2001). Chapter 9, pag.193

Fitting_Gompertz <- function(Age, lx, lix){
  # Where x is age
  
  
  l2 <- lix[Age==85]
  l1 <- lix[Age==80]
  l0 <- lix[Age==75]
  
  # Esitmate parameter b
  b_1  <- log(l2/l1)
  b_2  <- log(l1/l0)
  b_3  <- b_1 / b_2
  
  b <- b_3^.2
  
  # Estimate parameter a
  
  a_1  <- log(l1/l0)
  a_2  <- b ^75 * ((b^5) - 1)
  a  <- exp(a_1/a_2)
  
  # Estimate parameter C
  
  C_1 <- -b^75
  C_11 <- C_1 * log(a)
  C_2 <- exp(C_11)
  
  C <- l0 * C_2
  
  par_a <- a
  par_b <- b
  par_C <- C
  
  lx_fit <- rep(NA,length(Age))
  lx_fit <- lix
    for(i in 1:length(Age)){
      
      lx_fit[i] <- par_C * par_a^par_b^Age[i]
    }
  lx_fit <- round(lx_fit,0)   
  
  table <- data.frame(Age, lx, lix, lx_fit)
  
  return(table)
}


# ---------- Estimate Life table with lx

LT_lx <- function(Age, n, ax, qix, lix){
  
  i.openage <- length(lix)
  OPENAGE   <- i.openage - 1
  
  dix2 <- rep(NA,length(lix))
  
  dix2[1] <- lix[1] * qix[1]
  
  for(i in 1:length(lix)){
    
    dix2[i] <- lix[i] * qix[i]
  }
  
  dix2[Age==85] <- lix[Age==85] - lix[Age==90] 
  dix2[Age==90] <- lix[Age==90] - lix[Age==95] 
  dix2[Age==95] <- lix[Age==95] - lix[Age==100] 
  dix2[Age==100] <- lix[Age==100] - lix[Age==105] 
  dix2[Age==105] <- lix[Age==105] - lix[Age==110] 
  
  qix2 <- dix2/lix
  
  qix2[i.openage]       <- ifelse(is.na(qix2[i.openage]), NA, 1)
  qix2[Age==110]  <- 1    
  
  Lix2 <- rep(NA,length(lix))
  for(i in 1:OPENAGE){
    Lix2[i] <- lix[i+1]*n[i] + dix2[i]*ax[i]
  }
  Lix2[length(n)] <- 0
  
  Lix2[i.openage ]	    <- lix[i.openage ] * ax[i.openage ]
  
  # To estimate Tx by i case
  Tix2 	 <- c(rev(cumsum(rev(Lix2[1:OPENAGE]))),0) + Lix2[i.openage]
  
  # Life expectancy
  eix2 <- Tix2/lix
  
  base <- data.frame(Age = Age,
                     n =    n,
                     ax = ax,
                     qix = qix,
                     lix = lix,
                     dix2 = dix2,
                     qix2 = qix2,
                     Lix2 = Lix2,
                     Tix2 = Tix2,
                     eix2 = eix2)
  
  return(base)
  
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

# ------------------------  Edag ----------------------------
Edag.fun <- function(dx, ex, lx){
  Edag <- rev(cumsum(rev(dx*ex)))/lx
  return(Edag)
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


