#Data analysis/viz script - Phase 1 kestrel
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2022-06-18

#load tidyverse
library(tidyverse)
library(ggplot2)

#read in clean csvs
Phase1_Kestrel <- read_csv("data_clean/Phase1_Kestrel.csv")
Phase1_AvgTemp <- read_csv("data_QAQC/Phase1_AvgTemp.csv")

################################################################################
#add fake date to make graph
Phase1_AvgTemp <- Phase1_AvgTemp %>% 
  mutate(Date = as.Date("2021-10-15"))
Phase1_AvgTemp$DateTime <- paste(Phase1_AvgTemp$Date, " ", Phase1_AvgTemp$Time)
Phase1_AvgTemp$DateTime <- strptime(Phase1_AvgTemp$DateTime, format="%Y-%m-%d %H%M")

Phase1_AvgTemp$Phase <- as.factor(Phase1_AvgTemp$Phase)
Phase1_AvgTemp$Chamber <- as.factor(Phase1_AvgTemp$Chamber)

glimpse(Phase1_AvgTemp)

################################################################################

#Chamber 1 graph
Phase1_AvgTemp %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave) %>% 
  filter(Chamber == 1) %>% 
  ggplot(aes(x = as.POSIXct(DateTime),
             y = Temperature_avg,
             color = Kestrel)) +
  geom_point() +
  ylim(0, 40) +
  scale_x_datetime(date_labels = "%H%M") +
  xlab("Time") +
  ylab("Temperature (Celcius)") +
  labs(title = "Chamber 1 (PIPO PIED)\nAverage Daily Temperature Fluctuations",
       color = "Kestrel",
       fill = "Heatwave") +
  theme_minimal()

#Chamber 2 graph
Phase1_AvgTemp %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave) %>% 
  filter(Chamber == 2) %>% 
  ggplot(aes(x = as.POSIXct(DateTime),
             y = Temperature_avg,
             color = Kestrel)) +
  geom_point() +
  ylim(0, 40) +
  scale_x_datetime(date_labels = "%H%M") +
  xlab("Time") +
  ylab("Temperature (Celcius)") +
  labs(title = "Chamber 2 (PIFL PSME)\nAverage Daily Temperature Fluctuations",
       color = "Kestrel",
       fill = "Heatwave") +
  theme_minimal()

#Chamber 3 graph
Phase1_AvgTemp %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave) %>% 
  filter(Chamber == 3) %>% 
  ggplot(aes(x = as.POSIXct(DateTime),
             y = Temperature_avg,
             color = Kestrel)) +
  geom_point() +
  ylim(0, 40) +
  scale_x_datetime(date_labels = "%H%M") +
  xlab("Time") +
  ylab("Temperature (Celcius)") +
  labs(title = "Chamber 3 (PIEN)\nAverage Daily Temperature Fluctuations",
       color = "Kestrel",
       fill = "Heatwave") +
  theme_minimal()

################################################################################

# #my attempts to outline heatwave temps in red

# Phase1_AvgTemp %>% 
#   group_by(Phase, Chamber, Kestrel, Heatwave) %>% 
#   filter(Chamber == 1) %>% 
#   ggplot(aes(x = as.POSIXct(DateTime),
#              y = Temperature_avg,
#              color = Kestrel)) +
#   geom_point() +
#   geom_point(data = Chamber1_Graph_test,
#              size = 2) +
#   geom_point(data = Chamber1_Graph_test_HW,
#              size = 2,
#              color = "red",
#              shape = 21) +
#   ylim(0, 40) +
#   scale_x_datetime(date_labels = "%H%M") +
#   xlab("Time") +
#   ylab("Temperature (Celcius)") +
#   labs(title = "Chamber 1 (PIPO PIED)\nAverage Daily Temperature Fluctuations",
#        color = "Kestrel",
#        fill = "Heatwave") +
#   theme_minimal()


