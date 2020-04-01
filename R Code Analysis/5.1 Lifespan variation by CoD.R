# Project: EDSD Thesis
# Title:   LV measures for all HMD countries

# ---------------- Packes need and directory  -------------------
rm(list = ls())
setwd("~/Desktop/EDSD Thesis/R Analysis")

source("R Code/Packages Thesis.R")
source("R Code/Functions LV CoD.R")

# ---- Names of vectors
Country.name.vec <- c("Austria","Belgium","Bulgaria","Czech Republic",
                      "Denmark", "Estonia","Finland", "France",
                      "Germany", "Greece", "Hungary",  "Iceland",
                      "Ireland", "Italy", "Latvia", "Lithuania",
                      "Luxembourg", "Netherlands","Norway", "Poland",
                      "Portugal", "Slovakia","Slovenia", "Spain",
                      "Sweden", "Switzerland", "Ukraine","United Kingdom")

Sexes        <- c('Female','Male')

# ----- LV measures with HMD & WHO for causes of death 

get(load("Data/Data_combined.RData"))


Data_combined$Sex         <- as.factor(Data_combined$Sex)
levels(Data_combined$Sex) <- Sexes

Data_combined$Country  <- as.factor(Data_combined$Country.name)

table(Data_combined$Country, Data_combined$PopName)


# ----------- Cause for smoking related

Age2 <- c(0:110)

# ----------------------------------------------------------------------------------------------- #
# ------------------------------- Pr.C_S
# ----------------------------------------------------------------------------------------------- #

# -- Mean log devaition
MLD_Pr.C_S <- Data_combined[,list(L = meanlog_CoD(Age = Age2,
                                        dx = dx,
                                        ex = ex,  Pr = Pr.C_S)), by = list(Year,Sex,Country)]
MLD_Pr.C_S$Cause <- "Cancer_Smoking"

# -- Theil
Theil_Pr.C_S <- Data_combined[,list(T = Theil_CoD(Age = Age2,
                                        dx = dx,
                                        ex = ex,  Pr = Pr.C_S)), by = list(Year,Sex,Country)]
Theil_Pr.C_S$Cause <- "Cancer_Smoking"

# -- Variance
Var_Pr.C_S <- Data_combined[,list(V = LS_variance_CoD(Age = Age2,
                                        dx = dx,
                                        ex = ex,  Pr = Pr.C_S)), by = list(Year,Sex,Country)]
Var_Pr.C_S$Cause <- "Cancer_Smoking"

# -- SD
SD_Pr.C_S <- Data_combined[,list(SD = SD_SA_CoD(Age = Age2,
                                        dx = dx,
                                        ex = ex,  Pr = Pr.C_S)), by = list(Year,Sex,Country)]
SD_Pr.C_S$Cause <- "Cancer_Smoking"

CS_1 <- merge(MLD_Pr.C_S, Theil_Pr.C_S, by=c("Country", "Year",  "Sex", "Cause"))
CS_2 <- merge(CS_1, SD_Pr.C_S, by=c("Country", "Year",  "Sex", "Cause"))
CS_3 <- merge(CS_2, Var_Pr.C_S, by=c("Country", "Year",  "Sex", "Cause"))

Inequality_CS <- CS_3

# ----------------------------------------------------------------------------------------------- #
# ------------------------------- Pr.C_NS
# ----------------------------------------------------------------------------------------------- #

# -- Mean log devaition
MLD_Pr.C_NS <- Data_combined[,list(L = meanlog_CoD(Age = Age2,
                                         dx = dx,
                                         ex = ex,  Pr = Pr.C_NS)), by = list(Year,Sex,Country)]
MLD_Pr.C_NS$Cause <- "Cancer_No_Smoking"

# -- Theil
Theil_Pr.C_NS <- Data_combined[,list(T = Theil_CoD(Age = Age2,
                                         dx = dx,
                                         ex = ex,  Pr = Pr.C_NS)), by = list(Year,Sex,Country)]
Theil_Pr.C_NS$Cause <- "Cancer_No_Smoking"

# -- Variance
Var_Pr.C_NS <- Data_combined[,list(V = LS_variance_CoD(Age = Age2,
                                             dx = dx,
                                             ex = ex,  Pr = Pr.C_NS)), by = list(Year,Sex,Country)]
Var_Pr.C_NS$Cause <- "Cancer_No_Smoking"

# -- SD
SD_Pr.C_NS <- Data_combined[,list(SD = SD_SA_CoD(Age = Age2,
                                       dx = dx,
                                       ex = ex,  Pr = Pr.C_NS)), by = list(Year,Sex,Country)]
SD_Pr.C_NS$Cause <- "Cancer_No_Smoking"

CNS_1 <- merge(MLD_Pr.C_NS, Theil_Pr.C_NS, by=c("Country", "Year",  "Sex", "Cause"))
CNS_2 <- merge(CNS_1, SD_Pr.C_NS, by=c("Country", "Year",  "Sex", "Cause"))
CNS_3 <- merge(CNS_2, Var_Pr.C_NS, by=c("Country", "Year",  "Sex", "Cause"))

Inequality_CNS <- CNS_3

# ----------------------------------------------------------------------------------------------- #
# ------------------------------- Pr.Car & Diab
# ----------------------------------------------------------------------------------------------- #

# -- Mean log devaition
MLD_Pr.Car_and_Diab <- Data_combined[,list(L = meanlog_CoD(Age = Age2,
                                                 dx = dx,
                                                 ex = ex,  Pr = `Pr.Car & Diab`)), by = list(Year,Sex,Country)]
MLD_Pr.Car_and_Diab$Cause <- "Cardiovascular_and_Diabetes"

# -- Theil
Theil_Pr.Car_and_Diab <- Data_combined[,list(T = Theil_CoD(Age = Age2,
                                                 dx = dx,
                                                 ex = ex,  Pr = `Pr.Car & Diab`)), by = list(Year,Sex,Country)]
Theil_Pr.Car_and_Diab$Cause <- "Cardiovascular_and_Diabetes"

# -- Variance
Var_Pr.Car_and_Diab <- Data_combined[,list(V = LS_variance_CoD(Age = Age2,
                                                     dx = dx,
                                                     ex = ex,  Pr = `Pr.Car & Diab`)), by = list(Year,Sex,Country)]
Var_Pr.Car_and_Diab$Cause <- "Cardiovascular_and_Diabetes"

# -- SD
SD_Pr.Car_and_Diab <- Data_combined[,list(SD = SD_SA_CoD(Age = Age2,
                                               dx = dx,
                                               ex = ex,  Pr = `Pr.Car & Diab`)), by = list(Year,Sex,Country)]
SD_Pr.Car_and_Diab$Cause <- "Cardiovascular_and_Diabetes"

CCD_1 <- merge(MLD_Pr.Car_and_Diab, Theil_Pr.Car_and_Diab, by=c("Country", "Year","Sex", "Cause"))
CCD_2 <- merge(CCD_1, SD_Pr.Car_and_Diab, by=c("Country", "Year",  "Sex", "Cause"))
CCD_3 <- merge(CCD_2, Var_Pr.Car_and_Diab, by=c("Country", "Year",  "Sex", "Cause"))

Inequality_CCD <- CCD_3

# ----------------------------------------------------------------------------------------------- #
# ------------------------------- Pr.Resp_Inf
# ----------------------------------------------------------------------------------------------- #

# -- Mean log devaition
MLD_Pr.Resp_Inf <- Data_combined[,list(L = meanlog_CoD(Age = Age2,
                                             dx = dx,
                                             ex = ex,  Pr = `Pr.Resp_Inf`)), by = list(Year,Sex,Country)]
MLD_Pr.Resp_Inf$Cause <- "Resp_Infectious"

# -- Theil
Theil_Pr.Resp_Inf <- Data_combined[,list(T = Theil_CoD(Age = Age2,
                                             dx = dx,
                                             ex = ex,  Pr = `Pr.Resp_Inf`)), by = list(Year,Sex,Country)]
Theil_Pr.Resp_Inf$Cause <- "Resp_Infectious"

# -- Variance
Var_Pr.Resp_Inf <- Data_combined[,list(V = LS_variance_CoD(Age = Age2,
                                                 dx = dx,
                                                 ex = ex,  Pr = `Pr.Resp_Inf`)), by = list(Year,Sex,Country)]
Var_Pr.Resp_Inf$Cause <- "Resp_Infectious"

# -- SD
SD_Pr.Resp_Inf <- Data_combined[,list(SD = SD_SA_CoD(Age = Age2,
                                           dx = dx,
                                           ex = ex,  Pr = `Pr.Resp_Inf`)), by = list(Year,Sex,Country)]
SD_Pr.Resp_Inf$Cause <- "Resp_Infectious"

CRI_1 <- merge(MLD_Pr.Resp_Inf, Theil_Pr.Resp_Inf, by=c("Country", "Year","Sex", "Cause"))
CRI_2 <- merge(CRI_1, SD_Pr.Resp_Inf, by=c("Country", "Year",  "Sex", "Cause"))
CRI_3 <- merge(CRI_2, Var_Pr.Resp_Inf, by=c("Country", "Year",  "Sex", "Cause"))

Inequality_CRI <- CRI_3

# ----------------------------------------------------------------------------------------------- #
# ------------------------------- Pr.Resp_NoInf
# ----------------------------------------------------------------------------------------------- #

# -- Mean log devaition
MLD_Pr.Resp_NoInf <- Data_combined[,list(L = meanlog_CoD(Age = Age2,
                                               dx = dx,
                                               ex = ex,  Pr = `Pr.Resp_NoInf`)), by = list(Year,Sex,Country)]
MLD_Pr.Resp_NoInf$Cause <- "Resp_No_Infectious"

# -- Theil
Theil_Pr.Resp_NoInf <- Data_combined[,list(T = Theil_CoD(Age = Age2,
                                               dx = dx,
                                               ex = ex,  Pr = `Pr.Resp_NoInf`)), by = list(Year,Sex,Country)]
Theil_Pr.Resp_NoInf$Cause <- "Resp_No_Infectious"

# -- Variance
Var_Pr.Resp_NoInf <- Data_combined[,list(V = LS_variance_CoD(Age = Age2,
                                                   dx = dx,
                                                   ex = ex,  Pr = `Pr.Resp_NoInf`)), by = list(Year,Sex,Country)]
Var_Pr.Resp_NoInf$Cause <- "Resp_No_Infectious"

# -- SD
SD_Pr.Resp_NoInf <- Data_combined[,list(SD = SD_SA_CoD(Age = Age2,
                                             dx = dx,
                                             ex = ex,  Pr = `Pr.Resp_NoInf`)), by = list(Year,Sex,Country)]
SD_Pr.Resp_NoInf$Cause <- "Resp_No_Infectious"

CRNI_1 <- merge(MLD_Pr.Resp_NoInf, Theil_Pr.Resp_NoInf, by=c("Country", "Year","Sex", "Cause"))
CRNI_2 <- merge(CRNI_1, SD_Pr.Resp_NoInf, by=c("Country", "Year",  "Sex", "Cause"))
CRNI_3 <- merge(CRNI_2, Var_Pr.Resp_NoInf, by=c("Country", "Year",  "Sex", "Cause"))

Inequality_CRNI <- CRNI_3


# ----------------------------------------------------------------------------------------------- #
# ------------------------------- Pr.External
# ----------------------------------------------------------------------------------------------- #

# -- Mean log devaition
MLD_Pr.External <- Data_combined[,list(L = meanlog_CoD(Age = Age2,
                                             dx = dx,
                                             ex = ex,  Pr = `Pr.External`)), by = list(Year,Sex,Country)]
MLD_Pr.External$Cause <- "External"

# -- Theil
Theil_Pr.External <- Data_combined[,list(T = Theil_CoD(Age = Age2,
                                             dx = dx,
                                             ex = ex,  Pr = `Pr.External`)), by = list(Year,Sex,Country)]
Theil_Pr.External$Cause <- "External"

# -- Variance
Var_Pr.External <- Data_combined[,list(V = LS_variance_CoD(Age = Age2,
                                                 dx = dx,
                                                 ex = ex,  Pr = `Pr.External`)), by = list(Year,Sex,Country)]
Var_Pr.External$Cause <- "External"

# -- SD
SD_Pr.External <- Data_combined[,list(SD = SD_SA_CoD(Age = Age2,
                                           dx = dx,
                                           ex = ex,  Pr = `Pr.External`)), by = list(Year,Sex,Country)]
SD_Pr.External$Cause <- "External"

Ext_1 <- merge(MLD_Pr.External, Theil_Pr.External, by=c("Country", "Year","Sex", "Cause"))
Ext_2 <- merge(Ext_1, SD_Pr.External, by=c("Country", "Year",  "Sex", "Cause"))
Ext_3 <- merge(Ext_2, Var_Pr.External, by=c("Country", "Year",  "Sex", "Cause"))

Inequality_Ext <- Ext_3

# ----------------------------------------------------------------------------------------------- #
# ------------------------------- Pr.Other_Infec_NR
# ----------------------------------------------------------------------------------------------- #

# -- Mean log devaition
MLD_Pr.Other_Infec_NR <- Data_combined[,list(L = meanlog_CoD(Age = Age2,
                                                   dx = dx,
                                                   ex = ex,  Pr = `Pr.Other_Infec_NR`)), by = list(Year,Sex,Country)]
MLD_Pr.Other_Infec_NR$Cause <- "Other_Infec_NR"

# -- Theil
Theil_Pr.Other_Infec_NR <- Data_combined[,list(T = Theil_CoD(Age = Age2,
                                                   dx = dx,
                                                   ex = ex,  Pr = `Pr.Other_Infec_NR`)), by = list(Year,Sex,Country)]
Theil_Pr.Other_Infec_NR$Cause <- "Other_Infec_NR"

# -- Variance
Var_Pr.Other_Infec_NR <- Data_combined[,list(V = LS_variance_CoD(Age = Age2,
                                                       dx = dx,
                                                       ex = ex,  Pr = `Pr.Other_Infec_NR`)), by = list(Year,Sex,Country)]
Var_Pr.Other_Infec_NR$Cause <- "Other_Infec_NR"

# -- SD
SD_Pr.Other_Infec_NR <- Data_combined[,list(SD = SD_SA_CoD(Age = Age2,
                                                 dx = dx,
                                                 ex = ex,  Pr = `Pr.Other_Infec_NR`)), by = list(Year,Sex,Country)]
SD_Pr.Other_Infec_NR$Cause <- "Other_Infec_NR"


OINR_1 <- merge(MLD_Pr.Other_Infec_NR, Theil_Pr.Other_Infec_NR, by=c("Country", "Year","Sex", "Cause"))
OINR_2 <- merge(OINR_1, SD_Pr.Other_Infec_NR, by=c("Country", "Year",  "Sex", "Cause"))
OINR_3 <- merge(OINR_2, Var_Pr.Other_Infec_NR, by=c("Country", "Year",  "Sex", "Cause"))

Inequality_OINR <- OINR_3


Ineq_CoD <- rbind(Inequality_CCD, Inequality_CNS, Inequality_CRI,
                  Inequality_CRNI, Inequality_CS, Inequality_Ext,
                  Inequality_OINR)

save(Ineq_CoD, file = "Data/Ineq_CoD.RData")

