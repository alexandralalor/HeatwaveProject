#Data analysis/viz script - Phase 1 kestrel
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2022-06-17

#load tidyverse
library(tidyverse)
library(ggplot2)

#read in clean csvs
Phase1_Kestrel <- read_csv("data_QAQC/Phase1_Kestrel.csv")
Phase1_Chamber1_AvgTemp <- read_csv("data_QAQC/Phase1_Chamber1_AvgTemp.csv")
Phase1_Chamber2_AvgTemp <- read_csv("data_QAQC/Phase1_Chamber2_AvgTemp.csv")
Phase1_Chamber3_AvgTemp <- read_csv("data_QAQC/Phase1_Chamber2_AvgTemp.csv")

################################################################################
#add fake date to make graph
Phase1_Chamber1_AvgTemp <- Phase1_Chamber1_AvgTemp %>% 
  mutate(Date = as.Date("2021-10-15"))
Phase1_Chamber1_AvgTemp$DateTime <- paste(Phase1_Chamber1_AvgTemp$Date, " ", Phase1_Chamber1_AvgTemp$Time)
Phase1_Chamber1_AvgTemp$DateTime <- strptime(Phase1_Chamber1_AvgTemp$DateTime, format="%Y-%m-%d %H%M")

Phase1_Chamber2_AvgTemp <- Phase1_Chamber2_AvgTemp %>% 
  mutate(Date = as.Date("2021-10-15"))
Phase1_Chamber2_AvgTemp$DateTime <- paste(Phase1_Chamber2_AvgTemp$Date, " ", Phase1_Chamber2_AvgTemp$Time)
Phase1_Chamber2_AvgTemp$DateTime <- strptime(Phase1_Chamber2_AvgTemp$DateTime, format="%Y-%m-%d %H%M")

Phase1_Chamber3_AvgTemp <- Phase1_Chamber3_AvgTemp %>% 
  mutate(Date = as.Date("2021-10-15"))
Phase1_Chamber3_AvgTemp$DateTime <- paste(Phase1_Chamber3_AvgTemp$Date, " ", Phase1_Chamber3_AvgTemp$Time)
Phase1_Chamber3_AvgTemp$DateTime <- strptime(Phase1_Chamber3_AvgTemp$DateTime, format="%Y-%m-%d %H%M")

# Chamber1_Graph_test_HW <- Chamber1_Graph_test %>%
#   filter(Heatwave == "yes")


#graph
Phase1_Chamber3_AvgTemp %>% 
  group_by(Kestrel, Heatwave) %>% 
  ggplot(aes(x = as.POSIXct(DateTime),
             y = Temperature_avg,
             color = Kestrel)) +
  geom_point() +
  # geom_point(data = Chamber1_Graph_test,
  #            size = 2) +
  # geom_point(data = Chamber1_Graph_test_HW,
  #            size = 2,
  #            color = "red",
  #            shape = 21) +
  ylim(0, 40) +
  scale_x_datetime(date_labels = "%H%M") +
  xlab("Time") +
  ylab("Temperature (Celcius)") +
  labs(title = "Chamber 1 (PIPO PIED)\nAverage Daily Temperature Fluctuations",
       color = "Kestrel",
       fill = "Heatwave") +
  theme_minimal()



