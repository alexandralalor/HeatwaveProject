#Data analysis/viz script - Phase 1 kestrel
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2022-06-18

#load tidyverse
library(tidyverse)
library(ggplot2)

################################################################################
#Kestrel_AvgTemp
#Temperatures combined (daily temps), keeping Kestrel number intact for verification
################################################################################

#first, check that both kestrels were more or less equal

#read in clean csvs
Kestrel_AvgTemp <- read_csv("data/data_QAQC/Kestrel_AvgTemp.csv")

#add placeholder date to make graph
Kestrel_AvgTemp <- Kestrel_AvgTemp %>% 
  mutate(Date = as.Date("2021-10-15"))
Kestrel_AvgTemp$DateTime <- paste(Kestrel_AvgTemp$Date, " ", Kestrel_AvgTemp$Time)
Kestrel_AvgTemp$DateTime <- strptime(Kestrel_AvgTemp$DateTime, format="%Y-%m-%d %H%M")

Kestrel_AvgTemp$Phase <- as.factor(Kestrel_AvgTemp$Phase)
Kestrel_AvgTemp$Chamber <- as.factor(Kestrel_AvgTemp$Chamber)

glimpse(Kestrel_AvgTemp)


#Graphs!

#Chamber 1 graph
Kestrel_AvgTemp %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave) %>% 
  filter(Chamber == 1) %>% 
  ggplot(aes(x = as.POSIXct(DateTime),
             y = Temperature_avg,
             color = Kestrel)) +
  geom_point() +
  facet_wrap(~Phase) +
  ylim(0, 40) +
  scale_x_datetime(date_labels = "%H%M") +
  xlab("Time") +
  ylab("Temperature (Celsius)") +
  labs(title = "Chamber 1 (PIPO PIED)\nAverage Daily Temperature Fluctuations",
       color = "Kestrel",
       fill = "Heatwave") +
  theme_minimal()

#Chamber 2 graph
Kestrel_AvgTemp %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave) %>% 
  filter(Chamber == 2) %>% 
  ggplot(aes(x = as.POSIXct(DateTime),
             y = Temperature_avg,
             color = Kestrel)) +
  geom_point() +
  facet_wrap(~Phase) +
  ylim(0, 40) +
  scale_x_datetime(date_labels = "%H%M") +
  xlab("Time") +
  ylab("Temperature (Celsius)") +
  labs(title = "Chamber 2 (PIFL PSME)\nAverage Daily Temperature Fluctuations",
       color = "Kestrel",
       fill = "Heatwave") +
  theme_minimal()

#Chamber 3 graph
Kestrel_AvgTemp %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave) %>% 
  filter(Chamber == 3) %>% 
  ggplot(aes(x = as.POSIXct(DateTime),
             y = Temperature_avg,
             color = Kestrel)) +
  geom_point() +
  facet_wrap(~Phase) +
  ylim(0, 40) +
  scale_x_datetime(date_labels = "%H%M") +
  xlab("Time") +
  ylab("Temperature (Celsius)") +
  labs(title = "Chamber 3 (PIEN)\nAverage Daily Temperature Fluctuations",
       color = "Kestrel",
       fill = "Heatwave") +
  theme_minimal()

################################################################################
#Kestrel_AvgTemp_Sum
#Take average of both kestrels for summarized data (daily temps)
################################################################################

#now check summarized data, with the average of both kestrels

#read csv
Kestrel_AvgTemp_Sum <- read_csv("data/data_QAQC/Kestrel_AvgTemp_Sum.csv")

#add placeholder date to make graph
Kestrel_AvgTemp_Sum <- Kestrel_AvgTemp_Sum %>% 
  mutate(Date = as.Date("2021-10-15"))
Kestrel_AvgTemp_Sum$DateTime <- paste(Kestrel_AvgTemp_Sum$Date, " ", Kestrel_AvgTemp_Sum$Time)
Kestrel_AvgTemp_Sum$DateTime <- strptime(Kestrel_AvgTemp_Sum$DateTime, format="%Y-%m-%d %H%M")

Kestrel_AvgTemp_Sum$Phase <- as.factor(Kestrel_AvgTemp_Sum$Phase)
Kestrel_AvgTemp_Sum$Chamber <- as.factor(Kestrel_AvgTemp_Sum$Chamber)

glimpse(Kestrel_AvgTemp_Sum)


#Graphs!

#chamber 1 graph
Kestrel_AvgTemp_Sum %>%
  group_by(Phase, Chamber, Kestrel, Heatwave) %>%
  filter(Chamber == 1) %>%
  ggplot(aes(x = as.POSIXct(DateTime),
             y = Temperature_avg,
             color = Kestrel)) +
  geom_point() +
  geom_point(data = Kestrel_AvgTemp_Sum %>% filter(Chamber == 1, Heatwave == "no"),
             size = 2,
             color = "black",
             shape = 21) +
  geom_point(data = Kestrel_AvgTemp_Sum %>% filter(Chamber == 1, Heatwave == "yes"),
             size = 2,
             color = "red",
             shape = 21) +
  facet_wrap(~Phase) +
  ylim(0, 40) +
  scale_x_datetime(date_labels = "%H%M") +
  xlab("Time") +
  ylab("Temperature (Celsius)") +
  labs(title = "Chamber 1 (PIPO PIED)\nAverage Daily Temperature Fluctuations",
       color = "Kestrel") +
  geom_text(data = Kestrel_AvgTemp_Sum[94, ], 
            label = "Heatwave", color = "red", size = 3, vjust = 2) +
  geom_text(data = Kestrel_AvgTemp_Sum[46, ], 
            label = "Ambient", color = "black", size = 3, vjust = 3) +
  theme_minimal()


#chamber 2 graph
Kestrel_AvgTemp_Sum %>%
  group_by(Phase, Chamber, Kestrel, Heatwave) %>%
  filter(Chamber == 2) %>%
  ggplot(aes(x = as.POSIXct(DateTime),
             y = Temperature_avg,
             color = Kestrel)) +
  geom_point() +
  geom_point(data = Kestrel_AvgTemp_Sum %>% filter(Chamber == 2, Heatwave == "no"),
             size = 2,
             color = "black",
             shape = 21) +
  geom_point(data = Kestrel_AvgTemp_Sum %>% filter(Chamber == 2, Heatwave == "yes"),
             size = 2,
             color = "red",
             shape = 21) +
  facet_wrap(~Phase) +
  ylim(0, 40) +
  scale_x_datetime(date_labels = "%H%M") +
  xlab("Time") +
  ylab("Temperature (Celsius)") +
  labs(title = "Chamber 2 (PIFL PSME)\nAverage Daily Temperature Fluctuations",
       color = "Kestrel") +
  geom_text(data = Kestrel_AvgTemp_Sum[94, ], 
            label = "Heatwave", color = "red", size = 3, vjust = 8) +
  geom_text(data = Kestrel_AvgTemp_Sum[46, ], 
            label = "Ambient", color = "black", size = 3, vjust = 10) +
  theme_minimal()


#chamber 3 graph
Kestrel_AvgTemp_Sum %>%
  group_by(Phase, Chamber, Kestrel, Heatwave) %>%
  filter(Chamber == 3) %>%
  ggplot(aes(x = as.POSIXct(DateTime),
             y = Temperature_avg,
             color = Kestrel)) +
  geom_point() +
  geom_point(data = Kestrel_AvgTemp_Sum %>% filter(Chamber == 3, Heatwave == "no"),
             size = 2,
             color = "black",
             shape = 21) +
  geom_point(data = Kestrel_AvgTemp_Sum %>% filter(Chamber == 3, Heatwave == "yes"),
             size = 2,
             color = "red",
             shape = 21) +
  facet_wrap(~Phase) +
  ylim(0, 40) +
  scale_x_datetime(date_labels = "%H%M") +
  xlab("Time") +
  ylab("Temperature (Celsius)") +
  labs(title = "Chamber 3 (PIEN)\nAverage Daily Temperature Fluctuations",
       color = "Kestrel") +
  geom_text(data = Kestrel_AvgTemp_Sum[94, ], 
            label = "Heatwave", color = "red", size = 3, vjust = 9) +
  geom_text(data = Kestrel_AvgTemp_Sum[46, ], 
            label = "Ambient", color = "black", size = 3, vjust = 10) +
  theme_minimal()


#All chambers
Kestrel_AvgTemp_Sum_graph <- Kestrel_AvgTemp_Sum %>% 
  filter(Kestrel == "actual", Heatwave == "no")

Kestrel_AvgTemp_Sum_graph %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave) %>%
  ggplot(aes(x = as.POSIXct(DateTime),
              y = Temperature_avg,
              color = Chamber)) +
  geom_point() +
  facet_wrap(~Phase) +
  ylim(0, 40) +
  scale_x_datetime(date_labels = "%H%M") +
  xlab("Time") +
  ylab("Temperature (Celsius)") +
  labs(fill = "", tag = "(a)") +
  #labs(caption = "\nFIGURE S1a | Average Daily Temperatures Fluctuations of Growth Chambers") +
  theme_minimal() +
  theme(text = element_text(family = "serif", size = 10),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        strip.text.x = element_text(size = 10),
        plot.caption = element_text(hjust = 0,
                                    family = "serif",
                                    #face = "bold",
                                    size = 10))


################################################################################
#Kestrel_AvgTemp_Sum_Total
#Take average for overall temp summary (not daily)
################################################################################

#read csv
Kestrel_AvgTemp_Sum_Total <- read_csv("data/data_QAQC/Kestrel_AvgTemp_Sum_Total.csv")

#filter for actual temps
Kestrel_AvgTemp_Sum_Total_graph <- Kestrel_AvgTemp_Sum_Total %>% 
  filter(Kestrel == "actual")

#Graph!

#Bar graph
Kestrel_AvgTemp_Sum_Total_graph %>% 
  group_by(Phase, Heatwave) %>%
  ggplot(aes(x = Chamber,
             y = Temperature_avg,
             fill = Heatwave)) +
  geom_col(position= "dodge") +
  facet_wrap(~Phase) +
  ylim(0, 40) +
  ylab("Temperature (Celsius)") +
  labs(fill = "", tag = "(b)") +
  #labs(caption = "\nFIGURE S1b | Average Temperatures of Growth Chambers") +
  geom_text(label = Kestrel_AvgTemp_Sum_Total_graph$Temperature_avg,
            vjust = 1.5, position = position_dodge(0.9), color = "white") +
  #scale_fill_brewer(palette = "Set2") +
  scale_fill_discrete(direction = -1,
                      labels = c("Ambient", "Heatwave")) +
  theme_minimal() +
  theme(text = element_text(family = "serif", size = 10),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        strip.text.x = element_text(size = 10),
        plot.caption = element_text(hjust = 0,
                                    family = "serif",
                                    #face = "bold",
                                    size = 10))

################################################################################
# mean and range of temp differences in chambers
Kestrel_AvgTemp %>% 
  filter(Kestrel != "calculated") %>% 
  group_by(Phase, Chamber, Heatwave) %>% 
  summarise(mean = mean(Temperature_avg),
            min = min(Temperature_avg),
            max = max(Temperature_avg))
