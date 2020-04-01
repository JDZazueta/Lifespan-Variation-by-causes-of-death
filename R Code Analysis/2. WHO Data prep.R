# Project: EDSD Thesis
# Title:   Get WHO data

# ---------------- Packes need and directory  -------------------

rm(list = ls())

library(data.table)
library(reshape2)

setwd("~/Desktop/EDSD Thesis/R Analysis")

# --------------- Readgin the data from WHO ----------------------
Country.name.vec <- c("Austria","Belgium","Bulgaria","Czech Republic",
                      "Denmark", "Estonia","Finland", "France",
                      "Germany", "Greece", "Hungary",  "Iceland",
                      "Ireland", "Italy", "Latvia", "Lithuania",
                      "Luxembourg", "Netherlands","Norway", "Poland",
                      "Portugal", "Slovakia","Slovenia", "Spain",
                      "Sweden", "Switzerland", "Ukraine","United Kingdom")

# Create a vector with the countries' codes according to WHO
Country.code.vec <- c(4010, 4020, 4030, 4045, 
                      4050, 4055, 4070, 4080,
                      4085, 4140, 4150, 4160,
                      4170, 4180, 4186, 4188,
                      4190, 4210, 4220, 4230,
                      4240, 4274, 4276, 4280,
                      4290, 4300, 4308, 4303)

# ---- Now we ready the data for each ICD code and select the countries of interest of this study

# Get ICD-8
ICD8 <- data.table(read.table("Data/WHO Data 8-10/Morticd8.txt",header = T,sep = ',',stringsAsFactors = F))
ICD8 <- ICD8[(ICD8$Country==4010 |   ICD8$Country==4020 | ICD8$Country==4030 | 
                ICD8$Country==4045 | ICD8$Country==4050 | ICD8$Country==4055 | ICD8$Country==4070 | ICD8$Country==4080 |
                ICD8$Country==4085 | ICD8$Country==4140 | ICD8$Country==4150 | ICD8$Country==4160 | ICD8$Country==4170 |
                ICD8$Country==4180 | ICD8$Country==4186 | ICD8$Country==4188 | ICD8$Country==4190 | ICD8$Country==4210 |
                ICD8$Country==4220 | ICD8$Country==4230 | ICD8$Country==4240 | ICD8$Country==4274 |
                ICD8$Country==4276 | ICD8$Country==4280 | ICD8$Country==4290 | ICD8$Country==4300 | ICD8$Country==4308 |
                ICD8$Country==4303) & ICD8$Year>=1980]
ICD8$ICD <- 8

# Get ICD-9
ICD9 <- data.table(read.table("Data/WHO Data 8-10/Morticd9.txt",header = T,sep = ',',stringsAsFactors = F))
ICD9 <- ICD9[(ICD9$Country==4010 | ICD9$Country==4020 | ICD9$Country==4030 | 
                ICD9$Country==4045 | ICD9$Country==4050 | ICD9$Country==4055 | ICD9$Country==4070 | ICD9$Country==4080 |
                ICD9$Country==4085 | ICD9$Country==4140 | ICD9$Country==4150 | ICD9$Country==4160 | ICD9$Country==4170 |
                ICD9$Country==4180 | ICD9$Country==4186 | ICD9$Country==4188 | ICD9$Country==4190 | ICD9$Country==4210 |
                ICD9$Country==4220 | ICD9$Country==4230 | ICD9$Country==4240 | ICD9$Country==4274 |
                ICD9$Country==4276 | ICD9$Country==4280 | ICD9$Country==4290 | ICD9$Country==4300 | ICD9$Country==4308 |
                ICD9$Country==4303) & ICD9$Year>=1980]
ICD9$ICD <- 9

# Get ICD-10_1
ICD10_1 <- data.table(read.table("Data/WHO Data 8-10/MortIcd10_part1.txt",header = T,sep = ',',stringsAsFactors = F))
ICD10_1 <- ICD10_1[(ICD10_1$Country==4010 |  ICD10_1$Country==4020 | ICD10_1$Country==4030 | 
                      ICD10_1$Country==4045 | ICD10_1$Country==4050 | ICD10_1$Country==4055 | ICD10_1$Country==4070 | ICD10_1$Country==4080 |
                      ICD10_1$Country==4085 | ICD10_1$Country==4140 | ICD10_1$Country==4150 | ICD10_1$Country==4160 | ICD10_1$Country==4170 |
                      ICD10_1$Country==4180 | ICD10_1$Country==4186 | ICD10_1$Country==4188 | ICD10_1$Country==4190 | ICD10_1$Country==4210 |
                      ICD10_1$Country==4220 | ICD10_1$Country==4230 | ICD10_1$Country==4240 | ICD10_1$Country==4274 |
                      ICD10_1$Country==4276 | ICD10_1$Country==4280 | ICD10_1$Country==4290 | ICD10_1$Country==4300 | ICD10_1$Country==4308 |
                      ICD10_1$Country==4303) & ICD10_1$Year>=1980]
ICD10_1$ICD <- 10

# Get ICD-10_2
ICD10_2 <- data.table(read.table("Data/WHO Data 8-10/MortIcd10_part2.txt",header = T,sep = ',',stringsAsFactors = F))
ICD10_2 <- ICD10_2[(ICD10_2$Country==4010 | ICD10_2$Country==4020 | ICD10_2$Country==4030 | 
                      ICD10_2$Country==4045 | ICD10_2$Country==4050 | ICD10_2$Country==4055 | ICD10_2$Country==4070 | ICD10_2$Country==4080 |
                      ICD10_2$Country==4085 | ICD10_2$Country==4140 | ICD10_2$Country==4150 | ICD10_2$Country==4160 | ICD10_2$Country==4170 |
                      ICD10_2$Country==4180 | ICD10_2$Country==4186 | ICD10_2$Country==4188 | ICD10_2$Country==4190 | ICD10_2$Country==4210 |
                      ICD10_2$Country==4220 | ICD10_2$Country==4230 | ICD10_2$Country==4240 | ICD10_2$Country==4274 |
                      ICD10_2$Country==4276 | ICD10_2$Country==4280 | ICD10_2$Country==4290 | ICD10_2$Country==4300 | ICD10_2$Country==4308 |
                      ICD10_2$Country==4303) & ICD10_2$Year>=1980]
ICD10_2$ICD <- 10

# Now we combine the dataset of ICD10_1 & ICD10_2
ICD10 <- rbind(ICD10_1,ICD10_2)


# --- Now we combine the dataset
WHO_data <- rbind(ICD8,ICD9,ICD10)
WHO_data$Country.name <- as.factor(WHO_data$Country)
levels(WHO_data$Country.name) <- Country.name.vec


# Is necessary to eliminate cases with sex unspecified
# sex 1 is male, 2 is female, 9 is unspecified
WHO_data <- WHO_data[WHO_data$Sex != 9,]
WHO_data$Sex <- as.factor(WHO_data$Sex)
levels(WHO_data$Sex) <- c('m', 'f')

# We reaarange the data and eliminates the columns of Amind and Subdiv1
WHO_data <- WHO_data[,c(1,41,40,4:32)]

table(WHO_data$Country.name, WHO_data$Country)

save(WHO_data, file = "Data/WHO_Database.RData")

# I don't have ide what that command does
#gdata::keep(COD_Data, sure = T)