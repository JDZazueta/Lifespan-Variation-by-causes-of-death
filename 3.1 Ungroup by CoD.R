# Project: EDSD Thesis
# Title:   Ungroup Causes of death

# ---------------- Packes need and directory  -------------------
rm(list = ls())
setwd("~/Desktop/EDSD Thesis/R Analysis")

library(ggplot2)
library(data.table)
library(reshape2)
library(ungroup)
library(tidyverse)
library(parallelsugar)
library(testthat)
library(ungroup)
library(Xcode)

source("R Code/Function Rizzi.R")

# ----------- Preparing the data

#unique(Data$Country.name)
#unique(Data$Sex)
#unique(Data$Age)
#unique(Data$Cat)

# --- Get data group 1
Data_1 <- get(load("Data/Groups/Group_1.RData"))
Group_1_SAU <- Data_1[,list(Dx=Single_COD_fun(Dx), Age=0:110), by = list(Country,Country.name, ICD,Year,Sex,Cat)]
save(Group_1_SAU, file = "Data/Groups/Group_1_SAU.RData")


# --- Get data group 2
Data_2 <- get(load("Data/Groups/Group_2.RData"))
Group_2_SAU <- Data_2[,list(Dx=Single_COD_fun(Dx), Age=0:110), by = list(Country,Country.name, ICD,Year,Sex,Cat)]
save(Group_2_SAU, file = "Data/Groups/Group_2_SAU.RData")


# --- Get data group 3
Data_3 <- get(load("Data/Groups/Group_3.RData"))

table(Data_3$Country.name, Data_3$Country)

# Germany may be the problem
# 4085 4140
#Data_3 <- Data_3[Data_3$Country==4140 | Data_3$Country==4160]
#table(Data_3$Country.name, Data_3$Country)

#Group_3_SAU <- Data_3[,list(Dx=Single_COD_fun(Dx), Age=0:110), by = list(Country,Country.name, ICD,Year,Sex,Cat)]
#save(Group_3_SAU, file = "Data/Groups/Group_3_SAU.RData")

# --- Get data group 4
Data_4 <- get(load("Data/Groups/Group_4.RData"))
Group_4_SAU <- Data_4[,list(Dx=Single_COD_fun(Dx), Age=0:110), by = list(Country,Country.name, ICD,Year,Sex,Cat)]
save(Group_4_SAU, file = "Data/Groups/Group_4_SAU.RData")

# --- Get data group 5
Data_5 <- get(load("Data/Groups/Group_5.RData"))

table(Data_5$Country.name, Data_5$Country)
# We missing luxemburg and Poland

Data_5 <- Data_5[Data_5$Country==4210 | Data_5$Country==4220]
table(Data_5$Country.name, Data_5$Country)

Group_5_SAU <- Data_5[,list(Dx=Single_COD_fun(Dx), Age=0:110), by = list(Country,Country.name, ICD,Year,Sex,Cat)]
save(Group_5_SAU, file = "Data/Groups/Group_5_SAU.RData")


# --- Get data group 6
Data_6 <- get(load("Data/Groups/Group_6.RData"))
Group_6_SAU <- Data_6[,list(Dx=Single_COD_fun(Dx), Age=0:110), by = list(Country,Country.name, ICD,Year,Sex,Cat)]
save(Group_6_SAU, file = "Data/Groups/Group_6_SAU.RData")

# --- Get data group 6
Data_7 <- get(load("Data/Groups/Group_7.RData"))
Group_7_SAU <- Data_7[,list(Dx=Single_COD_fun(Dx), Age=0:110), by = list(Country,Country.name, ICD,Year,Sex,Cat)]
save(Group_7_SAU, file = "Data/Groups/Group_7_SAU.RData")


Single_Age_WHO <- rbind(Group_1_SAU, Group_2_SAU,
                        Group_4_SAU, Group_5_SAU,
                        Group_6_SAU, Group_7_SAU)

save(Single_Age_WHO, file = "Data/Single_Age_WHO.RData")


