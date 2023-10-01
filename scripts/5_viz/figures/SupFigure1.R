#Supplementary Figure 1 - temperature data
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2022-06-18

#load tidyverse
library(tidyverse)
library(ggplot2)
library(gridExtra) # for 2 panel graphs

################################################################################
#Phase1_Kestrel_AvgTemp_Sum
#Take average of both kestrels for summarized data (daily temps)
################################################################################

#check summarized data, with the average of both kestrels

#read csv
Phase1_Kestrel_AvgTemp_Sum <- read_csv("data_QAQC/Phase1_Kestrel_AvgTemp_Sum.csv")

#add placeholder date to make graph
Phase1_Kestrel_AvgTemp_Sum <- Phase1_Kestrel_AvgTemp_Sum %>% 
  mutate(Date = as.Date("2021-10-15"))
Phase1_Kestrel_AvgTemp_Sum$DateTime <- paste(Phase1_Kestrel_AvgTemp_Sum$Date, " ", Phase1_Kestrel_AvgTemp_Sum$Time)
Phase1_Kestrel_AvgTemp_Sum$DateTime <- strptime(Phase1_Kestrel_AvgTemp_Sum$DateTime, format="%Y-%m-%d %H%M")

Phase1_Kestrel_AvgTemp_Sum$Phase <- as.factor(Phase1_Kestrel_AvgTemp_Sum$Phase)
Phase1_Kestrel_AvgTemp_Sum$Chamber <- as.factor(Phase1_Kestrel_AvgTemp_Sum$Chamber)

glimpse(Phase1_Kestrel_AvgTemp_Sum)


#Graph all chambers daily fluctuations
Phase1_Kestrel_AvgTemp_Sum_graph <- Phase1_Kestrel_AvgTemp_Sum %>% 
  filter(Kestrel == "actual", Heatwave == "no")

a <- Phase1_Kestrel_AvgTemp_Sum_graph %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave) %>%
  ggplot(aes(x = as.POSIXct(DateTime),
             y = Temperature_avg,
             color = Chamber)) +
  geom_point() +
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
#Phase1_Kestrel_AvgTemp_Sum_Total
#Take average for overall temp summary (not daily)
################################################################################

#read csv
Phase1_Kestrel_AvgTemp_Sum_Total <- read_csv("data_QAQC/Phase1_Kestrel_AvgTemp_Sum_Total.csv")

#filter for actual temps
Phase1_Kestrel_AvgTemp_Sum_Total_graph <- Phase1_Kestrel_AvgTemp_Sum_Total %>% 
  filter(Kestrel == "actual")


#Graph - Bar graph of mean temps, heatwave and ambient
b <- Phase1_Kestrel_AvgTemp_Sum_Total_graph %>% 
  group_by(Heatwave) %>%
  ggplot(aes(x = Chamber,
             y = Temperature_avg,
             fill = Heatwave)) +
  geom_col(position= "dodge") +
  ylim(0, 40) +
  ylab("Temperature (Celsius)") +
  labs(fill = "", tag = "(b)") +
  #labs(caption = "\nFIGURE S1b | Average Temperatures of Growth Chambers") +
  geom_text(label = Phase1_Kestrel_AvgTemp_Sum_Total_graph$Temperature_avg,
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
################################################################################
# 2 panel grid (a and b) for Figure 1

grid.arrange(a, b, nrow=2)
