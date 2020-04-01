# Project: EDSD Thesis
# Title:   Get HMD data

# ---------------- Packes need  -------------------

rm(list = ls())
library(HMDHFDplus)
library(data.table)
setwd("~/Desktop/EDSD Thesis/R Analysis")


# Generate a country vector name to be compatible with WHO,
#but in HMD, the order is different, so we arrange different

Country.name.vec <- c("Austria","Belgium","Bulgaria","Belaruse",
                      "Switzerland", "Czech Republic","Germany", "Denmark",
                      "Spain", "Estonia",  "Finland", "France", "United Kingdom",
                      "Greece", "Croatia", "Hungary", "Ireland", "Iceland", 
                      "Italy", "Lithuania", "Luxembourg", "Latvia", "Netherlands",
                      "Norway", "Poland", "Portugal", "Russia Federation", "Slovakia",
                      "Slovenia",  "Sweden", "Ukraine")

#Country.name.vec <- c( "Denmark")

# ----------- Just ones takes time to download all

# get all countries in HMD
XYZ <- getHMDcountries()
# set your username for HMD
us <- "jdz_borboa@yahoo.com"
# set your password
pw <- "1562348573"

# get all the lifetables available from HMD
HMDL <- do.call(rbind,lapply(XYZ, function(x, us, pw){
  cat(x,"\n")
  Males        <- readHMDweb(x,"mltper_1x1",username=us,password=pw)
  Females      <- readHMDweb(x,"fltper_1x1",username=us,password=pw)
  Males$Sex    <- "m"
  Females$Sex  <- "f"
  CTRY         <- rbind(Females, Males)
  CTRY$PopName <- x
  CTRY    
}, us = us, pw = pw))

# convert to data.table
HMDL <- data.table(HMDL)

# Select countries and years of interes
# Select countries and years of interes
HMDL <- HMDL[(PopName=="AUT"| PopName=="BLR"| PopName=="BEL"| PopName=="BGR"| PopName=="HRV"|
                PopName=="CZE"| PopName=="DNK"| PopName=="EST"| PopName=="FIN"| PopName=="FRATNP"|
                PopName=="DEUTNP"| PopName=="GRC"| PopName=="HUN"| PopName=="ISL"| PopName=="IRL"|
                PopName=="ITA"| PopName=="LVA"| PopName=="LTU"| PopName=="LUX"| PopName=="NLD"|
                PopName=="NOR"| PopName=="POL"| PopName=="PRT"| PopName=="RUS"| PopName=="SVK"|
                PopName=="SVN"| PopName=="ESP"| PopName=="SWE"| PopName=="CHE"| PopName=="GBR_NP"|
                PopName=="UKR") & HMDL$Year >=1980]

# Select specific countries and year
HMDL$Country.name <- as.factor(HMDL$PopName)
levels(HMDL$Country.name) <- Country.name.vec

# We arrange the data in the way that we prefare
HMDL <- HMDL[,c(1,12,13,14,2:10)]

#HMDL <- HMDL[,c(1:13)]

# save the data
save(HMDL,file="Data/HMD_Data.RData")

# To check Label
table( HMDL$Country.name, HMDL$PopName)



