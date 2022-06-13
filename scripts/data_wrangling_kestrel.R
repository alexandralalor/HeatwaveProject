#Data wrangling script - Phase 1 kestrel
#Allie Lalor
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2022-06-11

#load tidyverse
library(tidyverse)
library(ggplot2)

#read in csv
Phase1_TempSettings <- read_csv("data_raw/kestrel/Phase1_TempSettings.csv")
Phase1_Chamber1_Kestrel1 <- read_csv("data_raw/kestrel/Phase1_Chamber1_Kestrel1.csv")
Phase1_Chamber1_Kestrel2 <- read_csv("data_raw/kestrel/Phase1_Chamber1_Kestrel2.csv")
# Phase1_Chamber2_Kestrel1 <- read_csv("data_raw/kestrel/Phase1_Chamber2_Kestrel1.csv")
# Phase1_Chamber2_Kestrel2 <- read_csv("data_raw/kestrel/Phase1_Chamber2_Kestrel2.csv")
# Phase1_Chamber3_Kestrel1 <- read_csv("data_raw/kestrel/Phase1_Chamber3_Kestrel1.csv")
# Phase1_Chamber3_Kestrel2 <- read_csv("data_raw/kestrel/Phase1_Chamber3_Kestrel2.csv")
# Phase1_Chamber4_Kestrel1 <- read_csv("data_raw/kestrel/Phase1_Chamber4_Kestrel1.csv")
# Phase1_Chamber4_Kestrel2 <- read_csv("data_raw/kestrel/Phase1_Chamber4_Kestrel2.csv")

#split up date and time
Phase1_Chamber1_Kestrel1 <- Phase1_Chamber1_Kestrel1 %>% 
  separate(Time, sep = " ",
           into = c("Date", "Time")) %>%
  mutate(Date = parse_datetime(Date,
                               format = "%m/%d/%Y"))

Phase1_Chamber1_Kestrel2 <- Phase1_Chamber1_Kestrel2 %>% 
  separate(Time, sep = " ",
           into = c("Date", "Time")) %>%
  mutate(Date = parse_datetime(Date,
                               format = "%m/%d/%Y"))

#Add Kestrel number
Phase1_Chamber1_Kestrel1 <- Phase1_Chamber1_Kestrel1 %>% 
  mutate(Kestrel = 1)
Phase1_Chamber1_Kestrel2 <- Phase1_Chamber1_Kestrel2 %>% 
  mutate(Kestrel = 2)

#Combine df
Phase1_Chamber1 <- rbind(Phase1_Chamber1_Kestrel1, Phase1_Chamber1_Kestrel2)

#Convert Phase, Chamber, Kestrel to categorical variables
Phase1_TempSettings$Chamber <- as.factor(Phase1_TempSettings$Chamber)
Phase1_TempSettings$Phase <- as.factor(Phase1_TempSettings$Phase)
Phase1_Chamber1$Chamber <- as.factor(Phase1_Chamber1$Chamber)
Phase1_Chamber1$Kestrel <- as.factor(Phase1_Chamber1$Kestrel)


#calcualted temperatures
Phase1_TempSettings %>% 
  group_by(Chamber) %>% 
  filter(Heatwave == "no") %>%
  ggplot(aes(x = Time,
             y = Temperature_C,
             color = Chamber)) +
  geom_point()
  


#compare temperatures
Phase1_Chamber1 %>% 
  group_by(Kestrel) %>% 
  filter(Time >= "8/27/2021 0:00" & Time <= "8/28/2021 0:00") %>% 
  ggplot(aes(x = Time,
             y = Temperature_C,
             color = Kestrel)) +
  geom_point()



