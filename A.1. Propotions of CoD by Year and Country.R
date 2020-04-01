# Project: EDSD Thesis
# Title:   Causes of death by country and year

# ---------------- Packes need and directory  -------------------------
rm(list = ls())
setwd("~/Desktop/EDSD Thesis/R Analysis")

source("R Code/Packages Thesis.R")
source("R Code/Functions LV CoD.R")

# ---------------- % Of deaths by cuases of death -----------------------

get(load("Data/Single_Age_WHO.RData"))
head(Single_Age_WHO)

# Get total deaths by age, sex, category, year.
SAC_Sum  <- Single_Age_WHO[, list(Dx=sum(Dx)), by =  list(Country,Country.name,ICD,Year,Sex,Age,Cat)]

# Get proportions of causes of death by age
SAC_Sum <- SAC_Sum[SAC_Sum$Age !="Total",]
SAC_Sum_prop <- SAC_Sum[, Dx.p := Dx/sum(Dx), by = list(Country,Country.name,ICD,Year,Sex,Age)]


# ----------------------------  All Countries ---------------------------

All <- SAC_Sum[, Dx.p := Dx/sum(Dx), by = list(Year,Sex,Age)]

All$Cat <- factor(All$Cat, levels = c(2,3,5,6,7,8,9),
                           labels = c("Cancer Smoking","Cancer No Smoking","Cardiovascular & Diabetes",
                                      "Respiratory Infectious","Respiratory No Infectious","External","Other No Infectious"))
All$Sex <- factor(All$Sex, levels = c("m","f"),
                           labels = c("Male","Female"))
All$Year <- as.factor(All$Year)

Euro_graph <- All %>% 
  filter(Year==1980 | Year==1990 | Year==2000 | Year==2010)

# -- Plot

All_Countries <- ggplot(Euro_graph, mapping = aes(x=Age, y=Dx.p, fill=Cat))+
  geom_bar(position = "stack", stat="identity") +
  theme_bw() +
  facet_grid(Year~Sex) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60,
                                70, 80, 90, 100, 110)) +
  theme(text = element_text(size = 15), legend.position="bottom")+
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "Europe: Proportions deaths by causes of death by age",
       subtitle = "(1980-2015)",
       caption = "Own elaboration WHO",
       tag = "",
       x = "Age",
       y = "Proportion",
       colour = "Causes of death")
All_Countries


# -------------------------  Prepare by countrie ---------------------------

SAC_Sum_prop$Cat <- factor(SAC_Sum_prop$Cat, levels = c(2,3,5,6,7,8,9),
                           labels = c("Cancer Smoking","Cancer No Smoking","Cardiovascular & Diabetes",
                                      "Respiratory Infectious","Respiratory No Infectious","External","Other No Infectious"))
SAC_Sum_prop$Sex <- factor(SAC_Sum_prop$Sex, levels = c("m","f"),
                           labels = c("Male","Female"))
SAC_Sum_prop$Year <- as.factor(SAC_Sum_prop$Year)

Period <- SAC_Sum_prop %>% 
  filter(Year==1980 | Year==1990 | Year==2000 | Year==2010)

# --------------- Austria 
Austria <- Period %>% 
  filter(Country.name=="Austria") 

AUT <- ggplot(Austria, mapping = aes(x=Age, y=Dx.p, fill=Cat))+
  geom_bar(position = "stack", stat="identity") +
  theme_bw() +
  facet_grid(Year~Sex) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60,
                                70, 80, 90, 100, 110)) +
  theme(text = element_text(size = 15), legend.position="bottom")+
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "Austria: Proportions deaths by causes of death by age",
       subtitle = "(1980-2015)",
       caption = "Own elaboration WHO",
       tag = "",
       x = "Age",
       y = "Proportion",
       colour = "Causes of death")
AUT

# --------------- Belgium 
Belgium <- Period %>% 
  filter(Country.name=="Belgium") 

BEL <- ggplot(Belgium, mapping = aes(x=Age, y=Dx.p, fill=Cat))+
  geom_bar(position = "stack", stat="identity") +
  theme_bw() +
  facet_grid(Year~Sex) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60,
                                70, 80, 90, 100, 110)) +
  theme(text = element_text(size = 15), legend.position="bottom")+
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "Belgium: Proportions deaths by causes of death by age",
       subtitle = "(1980-2015)",
       caption = "Own elaboration WHO",
       tag = "",
       x = "Age",
       y = "Proportion",
       colour = "Causes of death")
BEL

# --------------- Bulgaria 
Bulgaria <- Period %>% 
  filter(Country.name=="Bulgaria") 

BUL <- ggplot(Bulgaria, mapping = aes(x=Age, y=Dx.p, fill=Cat))+
  geom_bar(position = "stack", stat="identity") +
  theme_bw() +
  facet_grid(Year~Sex) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60,
                                70, 80, 90, 100, 110)) +
  theme(text = element_text(size = 15), legend.position="bottom")+
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "Bulgaria: Proportions deaths by causes of death by age",
       subtitle = "(1980-2015)",
       caption = "Own elaboration WHO",
       tag = "",
       x = "Age",
       y = "Proportion",
       colour = "Causes of death")
BUL

# --------------- Czech Republic 
CZRCH <- Period %>% 
  filter(Country.name=="Czech Republic") 

CZR <- ggplot(CZRCH, mapping = aes(x=Age, y=Dx.p, fill=Cat))+
  geom_bar(position = "stack", stat="identity") +
  theme_bw() +
  facet_grid(Year~Sex) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60,
                                70, 80, 90, 100, 110)) +
  theme(text = element_text(size = 15), legend.position="bottom")+
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "Czech Republic: Proportions deaths by causes of death by age",
       subtitle = "(1980-2015)",
       caption = "Own elaboration WHO",
       tag = "",
       x = "Age",
       y = "Proportion",
       colour = "Causes of death")
CZR

# --------------- Denmark 
Denmark <- Period %>% 
  filter(Country.name=="Denmark") 

DNK <- ggplot(Denmark, mapping = aes(x=Age, y=Dx.p, fill=Cat))+
  geom_bar(position = "stack", stat="identity") +
  theme_bw() +
  facet_grid(Year~Sex) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60,
                                70, 80, 90, 100, 110)) +
  theme(text = element_text(size = 15), legend.position="bottom")+
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "Denmark: Proportions deaths by causes of death by age",
       subtitle = "(1980-2015)",
       caption = "Own elaboration WHO",
       tag = "",
       x = "Age",
       y = "Proportion",
       colour = "Causes of death")
DNK

# --------------- Estonia 
Estonia <- Period %>% 
  filter(Country.name=="Estonia") 

EST <- ggplot(Estonia, mapping = aes(x=Age, y=Dx.p, fill=Cat))+
  geom_bar(position = "stack", stat="identity") +
  theme_bw() +
  facet_grid(Year~Sex) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60,
                                70, 80, 90, 100, 110)) +
  theme(text = element_text(size = 15), legend.position="bottom")+
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "Estonia: Proportions deaths by causes of death by age",
       subtitle = "(1980-2015)",
       caption = "Own elaboration WHO",
       tag = "",
       x = "Age",
       y = "Proportion",
       colour = "Causes of death")
EST

# --------------- Finland 
Finland <- Period %>% 
  filter(Country.name=="Finland") 

FIN <- ggplot(Finland, mapping = aes(x=Age, y=Dx.p, fill=Cat))+
  geom_bar(position = "stack", stat="identity") +
  theme_bw() +
  facet_grid(Year~Sex) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60,
                                70, 80, 90, 100, 110)) +
  theme(text = element_text(size = 15), legend.position="bottom")+
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "Finland: Proportions deaths by causes of death by age",
       subtitle = "(1980-2015)",
       caption = "Own elaboration WHO",
       tag = "",
       x = "Age",
       y = "Proportion",
       colour = "Causes of death")
FIN

# --------------- France 
France <- Period %>% 
  filter(Country.name=="France") 

FRN <- ggplot(France, mapping = aes(x=Age, y=Dx.p, fill=Cat))+
  geom_bar(position = "stack", stat="identity") +
  theme_bw() +
  facet_grid(Year~Sex) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60,
                                70, 80, 90, 100, 110)) +
  theme(text = element_text(size = 15), legend.position="bottom")+
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "France: Proportions deaths by causes of death by age",
       subtitle = "(1980-2015)",
       caption = "Own elaboration WHO",
       tag = "",
       x = "Age",
       y = "Proportion",
       colour = "Causes of death")
FRN

# --------------- Germany 
# --------------- Greece 
# --------------- Hungary 
# --------------- Iceland 

# --------------- Ireland 
Ireland <- Period %>% 
  filter(Country.name=="Ireland") 

IRE <- ggplot(Ireland, mapping = aes(x=Age, y=Dx.p, fill=Cat))+
  geom_bar(position = "stack", stat="identity") +
  theme_bw() +
  facet_grid(Year~Sex) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60,
                                70, 80, 90, 100, 110)) +
  theme(text = element_text(size = 15), legend.position="bottom")+
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "Ireland: Proportions deaths by causes of death by age",
       subtitle = "(1980-2015)",
       caption = "Own elaboration WHO",
       tag = "",
       x = "Age",
       y = "Proportion",
       colour = "Causes of death")
IRE

# --------------- Italy 
Italy <- Period %>% 
  filter(Country.name=="Italy") 

ITA <- ggplot(Italy, mapping = aes(x=Age, y=Dx.p, fill=Cat))+
  geom_bar(position = "stack", stat="identity") +
  theme_bw() +
  facet_grid(Year~Sex) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60,
                                70, 80, 90, 100, 110)) +
  theme(text = element_text(size = 15), legend.position="bottom")+
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "Italy: Proportions deaths by causes of death by age",
       subtitle = "(1980-2015)",
       caption = "Own elaboration WHO",
       tag = "",
       x = "Age",
       y = "Proportion",
       colour = "Causes of death")
ITA

# --------------- Latvia 
Latvia <- Period %>% 
  filter(Country.name=="Latvia") 

LAT <- ggplot(Latvia, mapping = aes(x=Age, y=Dx.p, fill=Cat))+
  geom_bar(position = "stack", stat="identity") +
  theme_bw() +
  facet_grid(Year~Sex) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60,
                                70, 80, 90, 100, 110)) +
  theme(text = element_text(size = 15), legend.position="bottom")+
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "Latvia: Proportions deaths by causes of death by age",
       subtitle = "(1980-2015)",
       caption = "Own elaboration WHO",
       tag = "",
       x = "Age",
       y = "Proportion",
       colour = "Causes of death")
LAT

# --------------- Lithuania 
Lithuania <- Period %>% 
  filter(Country.name=="Lithuania") 

LIT <- ggplot(Lithuania, mapping = aes(x=Age, y=Dx.p, fill=Cat))+
  geom_bar(position = "stack", stat="identity") +
  theme_bw() +
  facet_grid(Year~Sex) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60,
                                70, 80, 90, 100, 110)) +
  theme(text = element_text(size = 15), legend.position="bottom")+
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "Lithuania: Proportions deaths by causes of death by age",
       subtitle = "(1980-2015)",
       caption = "Own elaboration WHO",
       tag = "",
       x = "Age",
       y = "Proportion",
       colour = "Causes of death")
LIT

# --------------- Luxembourg 
# --------------- Netherlands 

Netherlands <- Period %>% 
  filter(Country.name=="Netherlands") 

NED <- ggplot(Netherlands, mapping = aes(x=Age, y=Dx.p, fill=Cat))+
  geom_bar(position = "stack", stat="identity") +
  theme_bw() +
  facet_grid(Year~Sex) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60,
                                70, 80, 90, 100, 110)) +
  theme(text = element_text(size = 15), legend.position="bottom")+
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "Netherlands: Proportions deaths by causes of death by age",
       subtitle = "(1980-2015)",
       caption = "Own elaboration WHO",
       tag = "",
       x = "Age",
       y = "Proportion",
       colour = "Causes of death")
NED

# --------------- Norway 
Norway <- Period %>% 
  filter(Country.name=="Norway") 

NOR <- ggplot(Norway, mapping = aes(x=Age, y=Dx.p, fill=Cat))+
  geom_bar(position = "stack", stat="identity") +
  theme_bw() +
  facet_grid(Year~Sex) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60,
                                70, 80, 90, 100, 110)) +
  theme(text = element_text(size = 15), legend.position="bottom")+
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "Norway: Proportions deaths by causes of death by age",
       subtitle = "(1980-2015)",
       caption = "Own elaboration WHO",
       tag = "",
       x = "Age",
       y = "Proportion",
       colour = "Causes of death")
NOR

# --------------- Poland 
# --------------- Portugal 
Portugal <- Period %>% 
  filter(Country.name=="Portugal") 

POR <- ggplot(Portugal, mapping = aes(x=Age, y=Dx.p, fill=Cat))+
  geom_bar(position = "stack", stat="identity") +
  theme_bw() +
  facet_grid(Year~Sex) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60,
                                70, 80, 90, 100, 110)) +
  theme(text = element_text(size = 15), legend.position="bottom")+
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "Portugal: Proportions deaths by causes of death by age",
       subtitle = "(1980-2015)",
       caption = "Own elaboration WHO",
       tag = "",
       x = "Age",
       y = "Proportion",
       colour = "Causes of death")
POR

# --------------- Slovakia 
Slovakia <- Period %>% 
  filter(Country.name=="Slovakia") 

SLV <- ggplot(Slovakia, mapping = aes(x=Age, y=Dx.p, fill=Cat))+
  geom_bar(position = "stack", stat="identity") +
  theme_bw() +
  facet_grid(Year~Sex) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60,
                                70, 80, 90, 100, 110)) +
  theme(text = element_text(size = 15), legend.position="bottom")+
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "Slovakia: Proportions deaths by causes of death by age",
       subtitle = "(1980-2015)",
       caption = "Own elaboration WHO",
       tag = "",
       x = "Age",
       y = "Proportion",
       colour = "Causes of death")
SLV

# --------------- Slovenia 
Slovenia <- Period %>% 
  filter(Country.name=="Slovenia") 

SLO <- ggplot(Slovenia, mapping = aes(x=Age, y=Dx.p, fill=Cat))+
  geom_bar(position = "stack", stat="identity") +
  theme_bw() +
  facet_grid(Year~Sex) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60,
                                70, 80, 90, 100, 110)) +
  theme(text = element_text(size = 15), legend.position="bottom")+
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "Slovenia: Proportions deaths by causes of death by age",
       subtitle = "(1980-2015)",
       caption = "Own elaboration WHO",
       tag = "",
       x = "Age",
       y = "Proportion",
       colour = "Causes of death")
SLO

# --------------- Spain 
Spain <- Period %>% 
  filter(Country.name=="Spain") 

SPN <- ggplot(Spain, mapping = aes(x=Age, y=Dx.p, fill=Cat))+
  geom_bar(position = "stack", stat="identity") +
  theme_bw() +
  facet_grid(Year~Sex) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60,
                                70, 80, 90, 100, 110)) +
  theme(text = element_text(size = 15), legend.position="bottom")+
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "Spain: Proportions deaths by causes of death by age",
       subtitle = "(1980-2015)",
       caption = "Own elaboration WHO",
       tag = "",
       x = "Age",
       y = "Proportion",
       colour = "Causes of death")
SPN

# --------------- Sweden 
Sweden <- Period %>% 
  filter(Country.name=="Sweden") 

SWE <- ggplot(Sweden, mapping = aes(x=Age, y=Dx.p, fill=Cat))+
  geom_bar(position = "stack", stat="identity") +
  theme_bw() +
  facet_grid(Year~Sex) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60,
                                70, 80, 90, 100, 110)) +
  theme(text = element_text(size = 15), legend.position="bottom")+
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "Sweden: Proportions deaths by causes of death by age",
       subtitle = "(1980-2015)",
       caption = "Own elaboration WHO",
       tag = "",
       x = "Age",
       y = "Proportion",
       colour = "Causes of death")
SWE

# --------------- Switzerland 
Switzerland <- Period %>% 
  filter(Country.name=="Switzerland") 

CHE <- ggplot(Switzerland, mapping = aes(x=Age, y=Dx.p, fill=Cat))+
  geom_bar(position = "stack", stat="identity") +
  theme_bw() +
  facet_grid(Year~Sex) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60,
                                70, 80, 90, 100, 110)) +
  theme(text = element_text(size = 15), legend.position="bottom")+
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "Switzerland: Proportions deaths by causes of death by age",
       subtitle = "(1980-2015)",
       caption = "Own elaboration WHO",
       tag = "",
       x = "Age",
       y = "Proportion",
       colour = "Causes of death")
CHE

# --------------- United Kingdom 
United_Kingdom <- Period %>% 
  filter(Country.name=="United Kingdom") 

UK <- ggplot(United_Kingdom, mapping = aes(x=Age, y=Dx.p, fill=Cat))+
  geom_bar(position = "stack", stat="identity") +
  theme_bw() +
  facet_grid(Year~Sex) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60,
                                70, 80, 90, 100, 110)) +
  theme(text = element_text(size = 15), legend.position="bottom")+
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "United Kingdom: Proportions deaths by causes of death by age",
       subtitle = "(1980-2015)",
       caption = "Own elaboration WHO",
       tag = "",
       x = "Age",
       y = "Proportion",
       colour = "Causes of death")
UK

# --------------- Ukraine 
Ukraine <- Period %>% 
  filter(Country.name=="Ukraine") 

UKR <- ggplot(Ukraine, mapping = aes(x=Age, y=Dx.p, fill=Cat))+
  geom_bar(position = "stack", stat="identity") +
  theme_bw() +
  facet_grid(Year~Sex) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60,
                                70, 80, 90, 100, 110)) +
  theme(text = element_text(size = 15), legend.position="bottom")+
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "Ukraine: Proportions deaths by causes of death by age",
       subtitle = "(1980-2015)",
       caption = "Own elaboration WHO",
       tag = "",
       x = "Age",
       y = "Proportion",
       colour = "Causes of death")
UKR







