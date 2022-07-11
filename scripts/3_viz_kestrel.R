#Data analysis/viz script - Phase 1 kestrel
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2022-06-18

#load tidyverse
library(tidyverse)
library(ggplot2)

################################################################################
#Phase1_Kestrel_AvgTemp
#Temperatures combined (daily temps), keeping Kestrel number intact for verification
################################################################################

#first, check that both kestrels were more or less equal

#read in clean csvs
Phase1_Kestrel_AvgTemp <- read_csv("data_QAQC/Phase1_Kestrel_AvgTemp.csv")

#add fake date to make graph
Phase1_Kestrel_AvgTemp <- Phase1_Kestrel_AvgTemp %>% 
  mutate(Date = as.Date("2021-10-15"))
Phase1_Kestrel_AvgTemp$DateTime <- paste(Phase1_Kestrel_AvgTemp$Date, " ", Phase1_Kestrel_AvgTemp$Time)
Phase1_Kestrel_AvgTemp$DateTime <- strptime(Phase1_Kestrel_AvgTemp$DateTime, format="%Y-%m-%d %H%M")

Phase1_Kestrel_AvgTemp$Phase <- as.factor(Phase1_Kestrel_AvgTemp$Phase)
Phase1_Kestrel_AvgTemp$Chamber <- as.factor(Phase1_Kestrel_AvgTemp$Chamber)

glimpse(Phase1_Kestrel_AvgTemp)


#Graphs!

#Chamber 1 graph
Phase1_Kestrel_AvgTemp %>% 
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
Phase1_Kestrel_AvgTemp %>% 
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
Phase1_Kestrel_AvgTemp %>% 
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
#Phase1_Kestrel_AvgTemp_Sum
#Take average of both kestrels for summarized data (daily temps)
################################################################################

#now check summarized data, with the average of both kestrels

#read csv
Phase1_Kestrel_AvgTemp_Sum <- read_csv("data_QAQC/Phase1_Kestrel_AvgTemp_Sum.csv")

#add fake date to make graph
Phase1_Kestrel_AvgTemp_Sum <- Phase1_Kestrel_AvgTemp_Sum %>% 
  mutate(Date = as.Date("2021-10-15"))
Phase1_Kestrel_AvgTemp_Sum$DateTime <- paste(Phase1_Kestrel_AvgTemp_Sum$Date, " ", Phase1_Kestrel_AvgTemp_Sum$Time)
Phase1_Kestrel_AvgTemp_Sum$DateTime <- strptime(Phase1_Kestrel_AvgTemp_Sum$DateTime, format="%Y-%m-%d %H%M")

Phase1_Kestrel_AvgTemp_Sum$Phase <- as.factor(Phase1_Kestrel_AvgTemp_Sum$Phase)
Phase1_Kestrel_AvgTemp_Sum$Chamber <- as.factor(Phase1_Kestrel_AvgTemp_Sum$Chamber)

glimpse(Phase1_Kestrel_AvgTemp_Sum)


#Graphs!

#chamber 1 graph
Phase1_Kestrel_AvgTemp_Sum %>%
  group_by(Phase, Chamber, Kestrel, Heatwave) %>%
  filter(Chamber == 1) %>%
  ggplot(aes(x = as.POSIXct(DateTime),
             y = Temperature_avg,
             color = Kestrel)) +
  geom_point() +
  geom_point(data = Phase1_Kestrel_AvgTemp_Sum %>% filter(Chamber == 1, Heatwave == "no"),
             size = 2,
             color = "black",
             shape = 21) +
  geom_point(data = Phase1_Kestrel_AvgTemp_Sum %>% filter(Chamber == 1, Heatwave == "yes"),
             size = 2,
             color = "red",
             shape = 21) +
  ylim(0, 40) +
  scale_x_datetime(date_labels = "%H%M") +
  xlab("Time") +
  ylab("Temperature (Celcius)") +
  labs(title = "Chamber 1 (PIPO PIED)\nAverage Daily Temperature Fluctuations",
       color = "Kestrel") +
  geom_text(data = Phase1_Kestrel_AvgTemp_Sum[94, ], 
            label = "Heatwave", color = "red", size = 3, vjust = 2) +
  geom_text(data = Phase1_Kestrel_AvgTemp_Sum[46, ], 
            label = "Ambient", color = "black", size = 3, vjust = 3) +
  theme_minimal()


#chamber 2 graph
Phase1_Kestrel_AvgTemp_Sum %>%
  group_by(Phase, Chamber, Kestrel, Heatwave) %>%
  filter(Chamber == 2) %>%
  ggplot(aes(x = as.POSIXct(DateTime),
             y = Temperature_avg,
             color = Kestrel)) +
  geom_point() +
  geom_point(data = Phase1_Kestrel_AvgTemp_Sum %>% filter(Chamber == 2, Heatwave == "no"),
             size = 2,
             color = "black",
             shape = 21) +
  geom_point(data = Phase1_Kestrel_AvgTemp_Sum %>% filter(Chamber == 2, Heatwave == "yes"),
             size = 2,
             color = "red",
             shape = 21) +
  ylim(0, 40) +
  scale_x_datetime(date_labels = "%H%M") +
  xlab("Time") +
  ylab("Temperature (Celcius)") +
  labs(title = "Chamber 2 (PIFL PSME)\nAverage Daily Temperature Fluctuations",
       color = "Kestrel") +
  geom_text(data = Phase1_Kestrel_AvgTemp_Sum[94, ], 
            label = "Heatwave", color = "red", size = 3, vjust = 8) +
  geom_text(data = Phase1_Kestrel_AvgTemp_Sum[46, ], 
            label = "Ambient", color = "black", size = 3, vjust = 10) +
  theme_minimal()


#chamber 3 graph
Phase1_Kestrel_AvgTemp_Sum %>%
  group_by(Phase, Chamber, Kestrel, Heatwave) %>%
  filter(Chamber == 3) %>%
  ggplot(aes(x = as.POSIXct(DateTime),
             y = Temperature_avg,
             color = Kestrel)) +
  geom_point() +
  geom_point(data = Phase1_Kestrel_AvgTemp_Sum %>% filter(Chamber == 3, Heatwave == "no"),
             size = 2,
             color = "black",
             shape = 21) +
  geom_point(data = Phase1_Kestrel_AvgTemp_Sum %>% filter(Chamber == 3, Heatwave == "yes"),
             size = 2,
             color = "red",
             shape = 21) +
  ylim(0, 40) +
  scale_x_datetime(date_labels = "%H%M") +
  xlab("Time") +
  ylab("Temperature (Celcius)") +
  labs(title = "Chamber 3 (PIEN)\nAverage Daily Temperature Fluctuations",
       color = "Kestrel") +
  geom_text(data = Phase1_Kestrel_AvgTemp_Sum[94, ], 
            label = "Heatwave", color = "red", size = 3, vjust = 9) +
  geom_text(data = Phase1_Kestrel_AvgTemp_Sum[46, ], 
            label = "Ambient", color = "black", size = 3, vjust = 10) +
  theme_minimal()


#All chambers
Phase1_Kestrel_AvgTemp_Sum_graph <- Phase1_Kestrel_AvgTemp_Sum %>% 
  filter(Kestrel == "actual", Heatwave == "no")

Phase1_Kestrel_AvgTemp_Sum_graph %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave) %>%
  ggplot(aes(x = as.POSIXct(DateTime),
              y = Temperature_avg,
              color = Chamber)) +
  geom_point() +
  ylim(0, 40) +
  scale_x_datetime(date_labels = "%H%M") +
  xlab("Time") +
  ylab("Temperature (Celcius)") +
  labs(title = "Average Daily Temperature Fluctuations") +
  theme_minimal()


################################################################################
#Phase1_Kestrel_AvgTemp_Sum_Total
#Take average for overall temp summary (not daily)
################################################################################

#read csv
Phase1_Kestrel_AvgTemp_Sum_Total <- read_csv("data_QAQC/Phase1_Kestrel_AvgTemp_Sum_Total.csv")

#filter for actual temps
Phase1_Kestrel_AvgTemp_Sum_Total_graph <- Phase1_Kestrel_AvgTemp_Sum_Total %>% 
  filter(Kestrel == "actual")

#Graph!

#Bar graph
Phase1_Kestrel_AvgTemp_Sum_Total_graph %>% 
  group_by(Heatwave) %>%
  ggplot(aes(x = Chamber,
             y = Temperature_avg,
             fill = Heatwave)) +
  geom_col(position= "dodge") +
  ylim(0, 40) +
  ylab("Temperature (Celcius)") +
  labs(title = "Average Temperatures") +
  geom_text(label = Phase1_Kestrel_AvgTemp_Sum_Total_graph$Temperature_avg,
            vjust = 1.5, position = position_dodge(0.9), color = "white") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

