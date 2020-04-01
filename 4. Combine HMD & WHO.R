# Project: EDSD Thesis
# Title:   Combine HMD & WHO

# ---------------- Packes need and directory  -------------------
rm(list = ls())

library(data.table)
library(reshape2)
library(tidyverse)

setwd("~/Desktop/EDSD Thesis/R Analysis")

# ---------------- Reading the data need it    -------------------

# Get HMD
get(load("Data/HMD_Data.RData"))

# Get WHO data
get(load("Data/Single_Age_WHO.RData"))


# --- We need to estimate the proportions of Dx

# Get total deaths by age, sex, category, year.
SAC_Sum  <- Single_Age_WHO[, list(Dx=sum(Dx)), by =  list(Country,Country.name,ICD,Year,Sex,Age,Cat)]

# Get proportions of causes of death by age
SAC_Sum <- SAC_Sum[SAC_Sum$Age !="Total",]

SAC_Sum_prop <- SAC_Sum[, Dx.p := Dx/sum(Dx), by = list(Country,Country.name,ICD,Year,Sex,Age)]



# Transfor the data of WHO where each column is  a proportion of Dx.prop
WHO_cast <- dcast(SAC_Sum_prop,Year+Sex+Country+Country.name+Age ~ Cat,value.var = 'Dx.p')
head(WHO_cast)

# Declare as factor in order to merge
WHO_cast$Year<-factor(WHO_cast$Year)
WHO_cast$Sex<-factor(WHO_cast$Sex)
WHO_cast$Country.name<-factor(WHO_cast$Country.name)
WHO_cast$Age<-factor(WHO_cast$Age)

WHO_cast <- WHO_cast[,c(1,2,4,5:12)]
head(WHO_cast)


# --- From the Human Mortality Database
# We select specific colums

HMD_data <- HMDL
head(HMD_data)

# Declare as factor in order to merge
HMD_data$Year<-factor(HMD_data$Year)
HMD_data$Sex<-factor(HMD_data$Sex)
HMD_data$Country.name<-factor(HMD_data$Country.name)
HMD_data$Age<-factor(HMD_data$Age)

# ---- Now we merge both dataset

Data_combined <- merge(HMD_data,WHO_cast,by = c("Year","Sex","Country.name","Age"))

# To check the combine
table(Data_combined$Country.name, Data_combined$PopName)

# We change the name of the proportion to the related cause of death
colnames(Data_combined)[14] <-"Pr.C_S"
colnames(Data_combined)[15] <-"Pr.C_NS"
colnames(Data_combined)[16] <-"Pr.Car_Diab"
colnames(Data_combined)[17] <-"Pr.Resp_Inf"
colnames(Data_combined)[18] <-"Pr.Resp_NoInf"
colnames(Data_combined)[19] <-"Pr.External"
colnames(Data_combined)[20] <-"Pr.Other_Infec_NR"

# Save final data
save(Data_combined, file = "Data/Data_combined.RData")

# We can check that everything is fine, estimation the sum of proportion
Data_combined$Sum <- Data_combined$Pr.C_S + Data_combined$Pr.C_NS + Data_combined$Pr.Car_Diab +
  Data_combined$Pr.Resp_Inf + Data_combined$Pr.Resp_NoInf + Data_combined$Pr.External +
  Data_combined$Pr.Other_Infec_NR

mean(Data_combined$Sum)
# if it is one is 1 everything is ok





