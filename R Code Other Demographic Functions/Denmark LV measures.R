# Project: EDSD Thesis
# Title:   LV measures by causes of death

# ---------------- Packes need and directory  -------------------
rm(list = ls())
setwd("~/Desktop/EDSD Thesis/R Analysis")

library(data.table)
library(reshape2)
library(tidyverse)
library(gghighlight)

source("R Code/Functions LV Thesis.R")

# ---- Names of vectors
Country.name.vec <- c("Austria","Belgium","Bulgaria","Czech Republic",
                      "Denmark", "Estonia","Finland", "France",
                      "Germany", "Greece", "Hungary",  "Iceland",
                      "Ireland", "Italy", "Latvia", "Lithuania",
                      "Luxembourg", "Netherlands","Norway", "Poland",
                      "Portugal", "Slovakia","Slovenia", "Spain",
                      "Sweden", "Switzerland", "Ukraine","United Kingdom")

Sexes        <- c('Female','Male')

# ----- LV measures with HMD original Dataset to get the overall

# - Get the data

get(load("Data/HMD_Data.RData"))

HMDL             <- HMDL[HMDL$Country %in% Country.name.vec,]
HMDL$Country     <- Country.name.vec[HMDL$Country]
HMDL$Sex         <- as.factor(HMDL$Sex)
levels(HMDL$Sex) <- Sexes

HMDL$Country  <- as.factor(HMDL$Country)

table(HMDL$Country, HMDL$PopName)

# -- Life expectancy at birth
e0 <- HMDL[,list(e0 = LE_B(Age = Age,ex = ex)), by = list(Year,Sex,Country)]
e0$Cause <- "Total"

# -- Edagger
edagger <- HMDL[,list(E = e.dagger.LT(fx=dx/100000,ex=ex,ax=ax)), by = list(Year,Sex,Country)]
edagger$Cause <- "Total"

# -- Standar Deviation
SD <- HMDL[,list(SD = SD_SA(Age = Age, dx = dx,ex = ex)), by = list(Year,Sex,Country)]
SD$Cause <- "Total"

# -- Variance
Var <- HMDL[,list(V = LS_variance(Age = Age, dx = dx,ex = ex)), by = list(Year,Sex,Country)]
Var$Cause <- "Total"

# -- Gini
Gini <- HMDL[,list(G = Gini.fun(x=Age, ndx = dx/100000,ex=ex,nax=ax)), by = list(Year,Sex,Country)]
Gini$Cause <- "Total"

# -- Mean log devaition
MLD <- HMDL[,list(L = meanlog(Age = Age, dx = dx,ex = ex)), by = list(Year,Sex,Country)]
MLD$Cause <- "Total"

# -- Theil
Theil <- HMDL[,list(T = Theil(Age = Age, dx = dx,ex = ex)), by = list(Year,Sex,Country)]
Theil$Cause <- "Total"

# --- Combine all data frames
G1 <- merge(e0, edagger, by=c("Country", "Year",  "Sex", "Cause"))
G2 <- merge(G1, SD, by=c("Country", "Year",  "Sex", "Cause"))
G3 <- merge(G2, Var, by=c("Country", "Year",  "Sex", "Cause"))
G4 <- merge(G3, Gini, by=c("Country", "Year",  "Sex", "Cause"))
G5 <- merge(G4, MLD, by=c("Country", "Year",  "Sex", "Cause"))
G6 <- merge(G5, Theil, by=c("Country", "Year",  "Sex", "Cause"))


# -------------- Example with DNK

DNK_Male_e0 <-  e0 %>% 
  filter(Country=="Denmark" & Sex=="Male")
plot(DNK_Male_e0$Year, DNK_Male_e0$e0)

DNK_Male_edag <- edagger %>% 
  filter(Country=="Denmark" & Sex=="Male")
plot(DNK_Male_edag$Year, DNK_Male_edag$E)

DNK_Male_V <- Var %>% 
  filter(Country=="Denmark" & Sex=="Male")
plot(DNK_Male_V$Year, DNK_Male_V$V)

DNK_Male_SD <- SD %>% 
  filter(Country=="Denmark" & Sex=="Male")
plot(DNK_Male_SD$Year, DNK_Male_SD$SD)

DNK_Male_gini <- Gini %>% 
  filter(Country=="Denmark" & Sex=="Male")
plot(DNK_Male_gini$Year, DNK_Male_gini$G)

DNK_Male_L <- MLD %>% 
  filter(Country=="Denmark" & Sex=="Male")
plot(DNK_Male_L$Year, DNK_Male_L$L)

DNK_Male_T <- Theil %>% 
  filter(Country=="Denmark" & Sex=="Male")
plot(DNK_Male_T$Year, DNK_Male_T$T)

G1 <- merge(DNK_Male_e0, DNK_Male_edag, by=c("Country", "Year",  "Sex", "Cause"))
G2 <- merge(G1, DNK_Male_SD, by=c("Country", "Year",  "Sex", "Cause"))
G3 <- merge(G2, DNK_Male_V, by=c("Country", "Year",  "Sex", "Cause"))
G4 <- merge(G3, DNK_Male_gini, by=c("Country", "Year",  "Sex", "Cause"))
G5 <- merge(G4, DNK_Male_L, by=c("Country", "Year",  "Sex", "Cause"))
G6 <- merge(G5, DNK_Male_T, by=c("Country", "Year",  "Sex", "Cause"))






