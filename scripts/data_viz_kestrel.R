#Data analysis/viz script - Phase 1 kestrel
#Allie Lalor
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2022-06-13

#load tidyverse
library(tidyverse)
library(ggplot2)

#read in csv
Phase1_TempSettings <- read_csv("data_clean/Phase1_TempSettings")
Phase1_Chamber1_Temps <- read_csv("data_clean/Phase1_Chamber1_Temps")


#compare temps using subset
Phase1_Chamber1_Temps_test <- Phase1_Chamber1_Temps %>% 
  filter(Date >= as.Date("2021-09-01")) %>% 
  group_by(Chamber, Kestrel, Time) %>% 
  summarize(Temperature_avg = mean(Temperature_C))

Phase1_TempSettings_test <- Phase1_TempSettings %>%
  group_by(Chamber, Kestrel, Time) %>% 
  filter(Heatwave == "no", Chamber == 1) %>% 
  summarize(Temperature_avg = mean(Temperature_C))

Phase1_Temps_test <- merge(Phase1_Chamber1_Temps_test, Phase1_TempSettings_test, all = T)


#add dummy date to make graph
Phase1_Temps_test <- Phase1_Temps_test %>% 
  mutate(Date = as.Date("2021-09-01"))
Phase1_Temps_test$DateTime <- paste(Phase1_Temps_test$Date, " ", Phase1_Temps_test$Time)
Phase1_Temps_test$DateTime <- strptime(Phase1_Temps_test$DateTime, format="%Y-%m-%d %H%M")


#graph
Phase1_Temps_test %>% 
  group_by(Kestrel) %>% 
  ggplot(aes(x = as.POSIXct(DateTime),
             y = Temperature_avg,
             color = Kestrel))+
  geom_point() +
  ylim(0, 40) +
  scale_x_datetime(date_labels = "%H%M") +
  xlab("Time")+
  ylab("Temperature (Celcius)") +
  labs(title = "Chamber 1 (PIPO PIED)\nAverage Daily Temperature Fluctuations") +
  theme_minimal()



