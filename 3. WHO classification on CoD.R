# Project: EDSD Thesis
# Title:   Classification of causes of death

# ---------------- Packes need and directory  -------------------
rm(list = ls())

library(data.table)
library(reshape2)
library(tidyverse)

setwd("~/Desktop/EDSD Thesis/R Analysis")

# ---------------- Reading the data -------------------
get(load("Data/WHO_Database.RData"))

# I took this comment from Jose Manuel Code from the Article on denmark

# We have to group causes according to the ICD classification
# for ICD7 we use the list 07A from WHO documentation for all years
# Maarten to classify causes of death following the Janssesn & Kunst paper
# and the WHO documentation.

#Classification made by Marteen (10 categories), if soesn't work regroup to 
# He provided the bridged codes

#2  Cancer, amenable to smoking
#3  Cancer, not amenable to smoking
#5  Cardiovascular & Diabetes mellitus (move to 5)
#6  Respiratory, infectious
#7  Respiratory, non-infectious
#8  External
#9  Other & Infectious, non-respiratory



# ICD 8 classification ----------------------------------------------------

ICD8.data    <- WHO_data[WHO_data$ICD==8,]
# good.codes8  <- unique(ICD8.data$Cause)[178:327]
# good.codes8  <- good.codes8[good.codes8!='A058']
# good.codes8  <- c(good.codes8, as.character(c(155,156,158,159,160,163,171,183,184,186:199,157)))
# # Drop those code that do not have an A to avoid duplicates
# ICD8.data <-   ICD8.data[ICD8.data$Cause %in% good.codes8,]

########## Bad codes
ICD8.data$Cat <- 10


########## category 1
ICD8.data[ICD8.data$Cause %in% c(paste0('A00',1:9),paste0('A0', 10:44)),]$Cat <- 9

########## category 2
ICD8.data[ICD8.data$Cause %in% c(paste0('A0',45:51),'A055','157','188','189'),]$Cat <- 2

########## category 3
ICD8.data[ICD8.data$Cause %in% c(paste0('A0',52:54),paste0('A0',56:57),
                                 paste0('A0',59:60),
                                 as.character(c(155,156,158,159,160,163,171,183,184,186,187,190:199))),]$Cat <- 3

########## category 4
ICD8.data[ICD8.data$Cause %in% c('A064'),]$Cat <- 5

########## category 5
ICD8.data[ICD8.data$Cause %in% c(paste0('A0',80:88)),]$Cat <- 5

########## category 6
ICD8.data[ICD8.data$Cause %in% c('A095',paste0('A0',89:92)),]$Cat <- 6

########## category 7
ICD8.data[ICD8.data$Cause %in% c('A093','A094','A096'),]$Cat <- 7

########## category 8
ICD8.data[ICD8.data$Cause %in% c(paste0('A',138:150)),]$Cat <- 8


########## Rest causes (category 9)
ICD8.data[ICD8.data$Cause %in% c(paste0('A0',61:63),paste0('A0',65:79),
                                 paste0('A0',97:99),paste0('A',100:137)),]$Cat <- 9

unique(ICD8.data$Cat)
ICD8.data <- ICD8.data[ICD8.data$Cat < 10,]



# ICD 9 classification ----------------------------------------------------

ICD9.data    <- WHO_data[WHO_data$ICD==9,]
#good.codes9  <- unique(ICD9.data$Cause)[73:392]

# Drop those code that do not have an A to avoid duplicates
#ICD9.data <-   ICD9.data[ICD9.data$Cause %in% good.codes9,]

########## Rest of bad codes
ICD9.data$Cat <- 10

########## category 1
ICD9.data[ICD9.data$Cause %in% c(paste0('B0',1:7),paste0('B', 184:185)),]$Cat <- 9

########## category 2
ICD9.data[ICD9.data$Cause %in% c(paste0('B0',90:94),'B08','B096','B100','B101','180','188','189'),]$Cat <- 2

########## category 3
ICD9.data[ICD9.data$Cause %in% c(paste0('B',121:126),paste0('B',13:14),'B095','B099','B109','B11','B13','B14', '179', '181',
                                 '182','183','184','185','186','187'),]$Cat <- 3

########## category 4
ICD9.data[ICD9.data$Cause %in% c('B181'),]$Cat <- 5

########## category 5
ICD9.data[ICD9.data$Cause %in% c(paste0('B',25:30)),]$Cat <- 5

########## category 6
ICD9.data[ICD9.data$Cause %in% c(paste0('B',310:312),paste0('B',320:322)),]$Cat <- 6

########## category 7
ICD9.data[ICD9.data$Cause %in% c(paste0('B',313:315),paste0('B',323:327),'B319','B329'),]$Cat <- 7

########## category 8
ICD9.data[ICD9.data$Cause %in% c(paste0('B',47:56)),]$Cat <- 8

########## category 9
ICD9.data[ICD9.data$Cause %in% c(paste0('B',15:17), 'B180', paste0('B',182:183),'B189', paste0('B',19:23),paste0('B',33:46)),]$Cat <- 9
unique(ICD9.data$Cat)
unique(ICD9.data[ICD9.data$Cat==10,]$Cause)
ICD9.data <- ICD9.data[ICD9.data$Cat!=10,]

# ICD 10 classification ----------------------------------------------------

ICD10.data      <- WHO_data[WHO_data$ICD==10,]
#get only the first 3 digits of the cause of death
sort(unique(ICD10.data$Cause))[2000:length(unique(ICD10.data$Cause))]
ICD10.data$Cause2 <- substr(ICD10.data$Cause,1,3)
ICD10.data$Cause3 <- substr(ICD10.data$Cause,4,4)
sort(unique(ICD10.data$Cause2))
sort(unique(ICD10.data$Cause3))

# Exclude the cause A000, is all causes in documentation
ICD10.data <- ICD10.data[ICD10.data$Cause != 'AAA',]

# bad codes
ICD10.data$Cat <- 10

########## category 1
ICD10.data[ICD10.data$Cause2 %in% c(paste0('A0',0:9),paste0('A', 10:99),
                                    paste0('B0', 0:9),paste0('B', 10:89),paste0('B', 99)),]$Cat <- 9

########## category 2
ICD10.data[ICD10.data$Cause2 %in% c(paste0('C0',0:9),paste0('C',10:21),paste0('C',25),paste0('C',30:34),'C53',
                                    paste0('C',64:68)),]$Cat <- 2

########## category 3
ICD10.data[ICD10.data$Cause2 %in% c(paste0('C',22:24),'C26',paste0('C',37:39),paste0('C',40:41),paste0('C',43:52),paste0('C',54:58),
                                    paste0('C',60:63),paste0('C',69:97)),]$Cat <- 3

########## category 4
ICD10.data[ICD10.data$Cause2 %in% c(paste0('E',10:14)),]$Cat <- 5

########## category 5
ICD10.data[ICD10.data$Cause2 %in% c(paste0('I0',0:9),paste0('I',10:99)),]$Cat <- 5

########## category 6
ICD10.data[ICD10.data$Cause2 %in% c(paste0('J0',0:6),paste0('J0',9),paste0('J',10:18),
                                    paste0('J',20:22),'J85','J86','J36'),]$Cat <- 6
ICD10.data[ICD10.data$Cause2=='J34' & ICD10.data$Cause3=='0',]$Cat <- 6
ICD10.data[ICD10.data$Cause2=='J39' & ICD10.data$Cause3=='0',]$Cat <- 6
ICD10.data[ICD10.data$Cause2=='J39' & ICD10.data$Cause3=='1',]$Cat <- 6

########## category 7
ICD10.data[ICD10.data$Cause2 %in% c(paste0('J',30:33),'J35','J37','J38',paste0('J',40:47),paste0('J',60:70),paste0('J',c(80,81,82)),
                                    paste0('J',90:99)),]$Cat <- 7
ICD10.data[ICD10.data$Cause2=='J34' & ICD10.data$Cause3=='1',]$Cat <- 7
ICD10.data[ICD10.data$Cause2=='J34' & ICD10.data$Cause3=='2',]$Cat <- 7
ICD10.data[ICD10.data$Cause2=='J34' & ICD10.data$Cause3=='3',]$Cat <- 7
ICD10.data[ICD10.data$Cause2=='J34' & ICD10.data$Cause3=='8',]$Cat <- 7
ICD10.data[ICD10.data$Cause2=='J39' & ICD10.data$Cause3=='2',]$Cat <- 7
ICD10.data[ICD10.data$Cause2=='J39' & ICD10.data$Cause3=='3',]$Cat <- 7
ICD10.data[ICD10.data$Cause2=='J39' & ICD10.data$Cause3=='8',]$Cat <- 7
ICD10.data[ICD10.data$Cause2=='J39' & ICD10.data$Cause3=='9',]$Cat <- 7

ICD10.data[ICD10.data$Cause2=='J84' & ICD10.data$Cause3=='0',]$Cat <- 7
ICD10.data[ICD10.data$Cause2=='J84' & ICD10.data$Cause3=='1',]$Cat <- 7

ICD10.data[ICD10.data$Cause2=='J84' & ICD10.data$Cause3=='8',]$Cat <- 7
ICD10.data[ICD10.data$Cause2=='J84' & ICD10.data$Cause3=='9',]$Cat <- 7

########## category 8
ICD10.data[ICD10.data$Cause2 %in% c(paste0('S0',0:9),paste0('T0',0:9),paste0('S',10:99),paste0('T',10:89),
                                    paste0('V',10:99),paste0('V0',1:9),
                                    paste0('W',10:99),paste0('W0',0:9),
                                    paste0('X',10:99),paste0('X0',0:9),
                                    paste0('Y',10:89),paste0('Y0',0:9)),]$Cat <- 8


########## Rest causes (category 9)
ICD10.data[ICD10.data$Cause2 %in% c(paste0('D0',0:9),paste0('D',10:48),paste0('D',50:89),
                                    paste0('E0',0:7),paste0('E',15:16),paste0('E',20:35),paste0('E',40:46),
                                    paste0('E',50:68),paste0('E',70:90),
                                    paste0('F0',0:9),paste0('F',10:99),
                                    paste0('G0',0:9),paste0('G',10:99),
                                    paste0('H0',0:9),paste0('H',10:59),paste0('H',60:95),
                                    paste0('K0',0:9),paste0('K',10:93),
                                    paste0('L0',0:9),paste0('L',10:99),
                                    paste0('M0',0:9),paste0('M',10:99),
                                    paste0('N0',0:9),paste0('N',10:99),
                                    paste0('O0',0:9),paste0('O',10:99),
                                    paste0('P0',0:9),paste0('P',10:96),
                                    paste0('Q0',0:9),paste0('Q',10:99),
                                    paste0('R0',0:9),paste0('R',10:99)),]$Cat <- 9
unique(ICD10.data$Cat)

##### Discard bad codes
check <- ICD10.data[ICD10.data$Cat==10,]
sort(unique(check$Cause))

ICD10.data <- ICD10.data[ICD10.data$Cat!=10,]

ICD10.data <- ICD10.data[,-c('Cause2','Cause3')]

# ----- Combine all databases on Cuases of deaths
WHO_COD_Data <- rbind(ICD8.data,ICD9.data,ICD10.data)

gdata::keep(WHO_COD_Data, sure = T)


##### Now play with ages
unique(WHO_COD_Data$Frmat)

# groups ages 1:4 for format 0
DT_COD.0       <- WHO_COD_Data[WHO_COD_Data$Frmat == 0,]
DT_COD.0$A_1_4 <- DT_COD.0$Deaths3 + DT_COD.0$Deaths4 + DT_COD.0$Deaths5 + DT_COD.0$Deaths6

# groups ages 1:4 for format 1
DT_COD.1       <- WHO_COD_Data[WHO_COD_Data$Frmat == 1,]
DT_COD.1$A_1_4 <- DT_COD.1$Deaths3 + DT_COD.1$Deaths4 + DT_COD.1$Deaths5 + DT_COD.1$Deaths6

# groups ages 1:4 for format 2
DT_COD.2       <- WHO_COD_Data[WHO_COD_Data$Frmat == 2,]
DT_COD.2$A_1_4 <- DT_COD.2$Deaths3

# rbind the 2 datasets
DT_COD <- rbind(DT_COD.0,DT_COD.1,DT_COD.2)

#reduce to variables needed (age < 85), until Deaths22 
DT_COD           <- DT_COD[,c('Country','Country.name', 'ICD', 'Year', 'Sex', 'Cat', 'Deaths1', 'Deaths2', 'A_1_4', paste0('Deaths',7:22))]
colnames(DT_COD) <- c('Country','Country.name', 'ICD', 'Year', 'Sex', 'Cat','Total',as.character(c(0,1,seq(5,80,5))))
DT_COD           <- DT_COD[with(DT_COD,order(Country,Country.name,Sex,Year,Cat))]


DT_COD.melt      <- melt(DT_COD, id.vars = c('Country','Country.name', 'ICD','Year','Sex','Cat'), variable.name = 'Age',value.name = 'Dx')

### Get total deaths by age, sex, category, year.
DT_COD.melt      <- DT_COD.melt[, list(Dx=sum(Dx)), by =  list(Country,Country.name,ICD,Year,Sex,Age,Cat)]

### get proportions of causes of death by age
DT_COD.melt      <- DT_COD.melt[DT_COD.melt$Age !="Total",]

DT_COD.melt      <- DT_COD.melt[, Dx.p := Dx/sum(Dx), by = list(Country,Country.name,ICD,Year,Sex,Age)]


# To ugrop easyly late

# group 1
Group_1 <- DT_COD.melt[DT_COD.melt$Country==4010 | DT_COD.melt$Country==4020 |
                       DT_COD.melt$Country==4030 | DT_COD.melt$Country==4045]

table(Group_1$Country.name, Group_1$Country)

# group 2
Group_2 <- DT_COD.melt[DT_COD.melt$Country==4050 | DT_COD.melt$Country==4055 |
                         DT_COD.melt$Country==4070 | DT_COD.melt$Country==4080]

table(Group_2$Country.name, Group_2$Country)

# group 3
Group_3 <- DT_COD.melt[DT_COD.melt$Country==4085 | DT_COD.melt$Country==4140 |
                         DT_COD.melt$Country==4150 | DT_COD.melt$Country==4160]

table(Group_3$Country.name, Group_3$Country)

# group 4
Group_4 <- DT_COD.melt[DT_COD.melt$Country==4170 | DT_COD.melt$Country==4180 |
                         DT_COD.melt$Country==4186 | DT_COD.melt$Country==4188]

table(Group_4$Country.name, Group_4$Country)


# group 5
Group_5 <- DT_COD.melt[DT_COD.melt$Country==4190 | DT_COD.melt$Country==4210 |
                         DT_COD.melt$Country==4220 | DT_COD.melt$Country==4230]

table(Group_5$Country.name, Group_5$Country)

# group 6
Group_6 <- DT_COD.melt[DT_COD.melt$Country==4240 | DT_COD.melt$Country==4274 |
                         DT_COD.melt$Country==4276 | DT_COD.melt$Country==4280]

table(Group_6$Country.name, Group_6$Country)

# group 7
Group_7 <- DT_COD.melt[DT_COD.melt$Country==4290 | DT_COD.melt$Country==4300 |
                         DT_COD.melt$Country==4308 | DT_COD.melt$Country==4303]

table(Group_7$Country.name, Group_7$Country)

# -- Save the data

save(Group_1, file = "Data/Groups/Group_1.RData")
save(Group_2, file = "Data/Groups/Group_2.RData")
save(Group_3, file = "Data/Groups/Group_3.RData")
save(Group_4, file = "Data/Groups/Group_4.RData")
save(Group_5, file = "Data/Groups/Group_5.RData")
save(Group_6, file = "Data/Groups/Group_6.RData")
save(Group_7, file = "Data/Groups/Group_7.RData")


