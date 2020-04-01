# Project: EDSD Thesis
# Title:   Graphs LV measures by Causes of death

# ---------------- Packes need and directory  -------------------
rm(list = ls())
setwd("~/Desktop/EDSD Thesis/R Analysis")

source("R Code/Packages Thesis.R")
source("R Code/Functions LV CoD.R")

# ---------------- Combine both datasets  -----------------------

get(load("Data/Ineq_HMD.RData"))

# In Ineq HMD we have life expetancy at birth as well, gini and edager
Ineq_HMD_2 <- Ineq_HMD %>% 
  select(Country, Year, Sex, Cause, SD, V, L, T)

# Inequality by causes of death
get(load("Data/Ineq_CoD.RData"))

# Combine both data soruces
Ineq_Europe <- rbind(Ineq_HMD_2, Ineq_CoD)
save(Ineq_Europe, file = "Data/Ineq_Europe.RData")

# ----------------------------------------------------------------------------------------------- #
# ---------------- Graphs HMD life expecntancy and lifespan variation
# ----------------------------------------------------------------------------------------------- #

# Edagger
Graph_E <- ggplot(Ineq_HMD, mapping = aes(x=e0, y=E, color=Year)) + 
  #geom_line(aes(group = Cause,size= Cause),show.legend = T, size=1.2)+
  geom_point() +
  theme_bw() +
  facet_grid(~Sex)+
  scale_y_continuous(expression(e^"\u2020"), limits = c(9,18)) +
  scale_x_continuous(breaks = c(55, 60, 65, 70,
                                75, 80, 85, 90)) +
  labs(title = expression(paste('Overall Life disparity (',e^"\u2020",') and life expectancy at birth')),
    subtitle = "(1980-2015)",
    caption = "Own elaboration HMD",
    tag = "",
    x = "e0",
    y = expression(e^"\u2020"),
    colour = "Year")
Graph_E

# SD
Graph_SD <- ggplot(Ineq_HMD, mapping = aes(x=e0, y=SD, color=Year)) + 
  #geom_line(aes(group = Cause,size= Cause),show.legend = T, size=1.2)+
  geom_point() +
  theme_bw() +
  facet_grid(~Sex)+
  scale_y_continuous("SD", limits = c(10,22)) +
  scale_x_continuous(breaks = c(55, 60, 65, 70,
                                75, 80, 85, 90)) +
  labs(title = 'Overall Life disparity: SD and life expectancy at birth',
       subtitle = "(1980-2015)",
       caption = "Own elaboration HMD",
       tag = "",
       x = "e0",
       y = "SD",
       colour = "Year")
Graph_SD

# Gini
Graph_G <- ggplot(Ineq_HMD, mapping = aes(x=e0, y=G, color=Year)) + 
  #geom_line(aes(group = Cause,size= Cause),show.legend = T, size=1.2)+
  geom_point() +
  theme_bw() +
  facet_grid(~Sex)+
  scale_y_continuous("Gini", limits = c(0.05,.22)) +
  scale_x_continuous(breaks = c(55, 60, 65, 70,
                                75, 80, 85, 90)) +
  labs(title = 'Overall Life disparity: Gini and life expectancy at birth',
       subtitle = "(1980-2015)",
       caption = "Own elaboration HMD",
       tag = "",
       x = "e0",
       y = "Gini",
       colour = "Year")
Graph_G

# Mean log
Graph_L <- ggplot(Ineq_HMD, mapping = aes(x=e0, y=L, color=Year)) + 
  #geom_line(aes(group = Cause,size= Cause),show.legend = T, size=1.2)+
  geom_point() +
  theme_bw() +
  facet_grid(~Sex)+
  scale_y_continuous("Mean log deviation", limits = c(0.01,.07)) +
  scale_x_continuous(breaks = c(55, 60, 65, 70,
                                75, 80, 85, 90)) +
  labs(title = 'Overall Life disparity: Mean log deviation and life expectancy at birth',
       subtitle = "(1980-2015)",
       caption = "Own elaboration HMD",
       tag = "",
       x = "e0",
       y = "L",
       colour = "Year")
Graph_L

# Theil
Graph_T <- ggplot(Ineq_HMD, mapping = aes(x=e0, y=T, color=Year)) + 
  #geom_line(aes(group = Cause,size= Cause),show.legend = T, size=1.2)+
  geom_point() +
  theme_bw() +
  facet_grid(~Sex)+
  scale_y_continuous("Theil", limits = c(0.001,.07)) +
  scale_x_continuous(breaks = c(55, 60, 65, 70,
                                75, 80, 85, 90)) +
  labs(title = 'Overall Life disparity: Theil and life expectancy at birth',
       subtitle = "(1980-2015)",
       caption = "Own elaboration HMD",
       tag = "",
       x = "e0",
       y = "L",
       colour = "Year")
Graph_T


# ----------------------------------------------------------------------------------------------- #
# -------------- Graphs HMD & WHO life expecntancy and lifespan variation by CoD
# ----------------------------------------------------------------------------------------------- #


# SD

Ineq_Europe$Year=as.numeric(levels(Ineq_Europe$Year)) [Ineq_Europe$Year]

#DNK <- Ineq_Europe[Ineq_Europe$Country=="United Kingdom"]
DNK <- Ineq_Europe[Country=="Denmark"]

table(Ineq_Europe$Country)

# ---- Absolute measures
Graph_V_CoD <- ggplot(DNK, mapping = aes(x=Year, y=V, color=Cause)) + 
  geom_line(aes(group = Cause,size= Cause),show.legend = T, size=1.2)+
  theme_bw() +
  theme(legend.position="bottom") +
  facet_grid(~Sex)+
  scale_x_continuous(breaks = c(1980, 1985, 1990, 1995,
                                2000, 2005, 2010, 2015)) +
  labs(title = 'Denmark: Variance by causes of death & sex',
       subtitle = "(1980-2015)",
       caption = "Own elaboration HMD",
       tag = "",
       x = "Year",
       y = "Variance",
       colour = "Cause of death")
Graph_V_CoD


Graph_SD_CoD <- ggplot(DNK, mapping = aes(x=Year, y=SD, color=Cause)) + 
  geom_line(aes(group = Cause,size= Cause),show.legend = T, size=1.2)+
  theme_bw() +
  theme(legend.position="bottom") +
  facet_grid(~Sex)+
  scale_x_continuous(breaks = c(1980, 1985, 1990, 1995,
                                2000, 2005, 2010, 2015)) +
  labs(title = 'Denmark: Standar Deviation by causes of death & sex',
       subtitle = "(1980-2015)",
       caption = "Own elaboration HMD",
       tag = "",
       x = "Year",
       y = "Variance",
       colour = "Cause of death")
Graph_SD_CoD

# ---- Relative measures

Graph_T_CoD <- ggplot(DNK, mapping = aes(x=Year, y=T, color=Cause)) + 
  geom_line(aes(group = Cause,size= Cause),show.legend = T, size=1.2)+
  theme_bw() +
  theme(legend.position="bottom") +
  facet_grid(~Sex)+
  scale_x_continuous(breaks = c(1980, 1985, 1990, 1995,
                                2000, 2005, 2010, 2015)) +
  labs(title = 'Denmark: Theil by causes of death & sex',
       subtitle = "(1980-2015)",
       caption = "Own elaboration HMD",
       tag = "",
       x = "Year",
       y = "Variance",
       colour = "Cause of death")
Graph_T_CoD

Graph_L_CoD <- ggplot(DNK, mapping = aes(x=Year, y=L, color=Cause)) + 
  geom_line(aes(group = Cause,size= Cause),show.legend = T, size=1.2)+
  theme_bw() +
  theme(legend.position="bottom") +
  facet_grid(~Sex)+
  scale_x_continuous(breaks = c(1980, 1985, 1990, 1995,
                                2000, 2005, 2010, 2015)) +
  labs(title = 'Denmark: Mean-log deviation by causes of death & sex',
       subtitle = "(1980-2015)",
       caption = "Own elaboration HMD",
       tag = "",
       x = "Year",
       y = "Variance",
       colour = "Cause of death")
Graph_L_CoD

# new graphs soon







