#Data viz - porometer
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-07-07
#Last updated: 2022-07-13

#load tidyverse
library(tidyverse)

#read in csvs
Phase1_Data_Porometer <- read_csv("data_QAQC/Phase1_Data_Porometer.csv")

#check out data
glimpse(Phase1_Data_Porometer)

#convert variables
#if needed, convert time from double <dbl> to character <chr>
Phase1_Data_Porometer <- Phase1_Data_Porometer %>% 
  mutate(Time = ifelse(Time < 1000 & Time > 30, paste0("0", Phase1_Porometer$Time), 
                       ifelse(Time == 30, paste0("00", Phase1_Porometer$Time), 
                              ifelse(Time == 0, paste0("000", Phase1_Porometer$Time), Time))))
Phase1_Data_Porometer$DateTime <- as.POSIXct(Phase1_Data_Porometer$DateTime, format = "%m/%d/%Y %H:%M")
Phase1_Data_Porometer$Date <- as.Date(Phase1_Data_Porometer$Date, format = "%m/%d/%Y")

Phase1_Data_Porometer$Phase <- as.factor(Phase1_Data_Porometer$Phase)
Phase1_Data_Porometer$Chamber <- as.factor(Phase1_Data_Porometer$Chamber)
Phase1_Data_Porometer$ScientificName <- as.factor(Phase1_Data_Porometer$ScientificName)
Phase1_Data_Porometer$CommonName <- as.factor(Phase1_Data_Porometer$CommonName)
Phase1_Data_Porometer$Species <- as.factor(Phase1_Data_Porometer$Species)
Phase1_Data_Porometer$Treatment_temp <- as.factor(Phase1_Data_Porometer$Treatment_temp)
Phase1_Data_Porometer$Treatment_water <- as.factor(Phase1_Data_Porometer$Treatment_water)
Phase1_Data_Porometer$PorometerSubset <- as.factor(Phase1_Data_Porometer$PorometerSubset)
Phase1_Data_Porometer$Dead <- as.factor(Phase1_Data_Porometer$Dead)
Phase1_Data_Porometer$Dead_Count <- as.factor(Phase1_Data_Porometer$Dead_Count)
Phase1_Data_Porometer$Heatwave_graph <- as.factor(Phase1_Data_Porometer$Heatwave_graph)
Phase1_Data_Porometer$Heatwave <- as.factor(Phase1_Data_Porometer$Heatwave)


################################################################################
#Find average porometer data for graphing
################################################################################

#filter for porometer data
Phase1_Data_Porometer <- Phase1_Data_Porometer %>% 
  filter(PorometerSubset == "yes", !is.na(Porometer))

#average data 
Phase1_Data_Porometer_Avg <- Phase1_Data_Porometer %>%
  group_by(ScientificName, CommonName, Species, Week, Treatment_temp, Treatment_water) %>% 
  summarize(Dead_Count = sum(Dead_Count),
            PercentDead = 100*(Dead_Count/20),
            Porometer = mean(Porometer),
            Temperature_C = mean(Temperature_C),
            LeafSensor_PercentRH = mean(LeafSensor_PercentRH),
            FilterSensor_PercentRH = mean(FilterSensor_PercentRH)) %>% 
  arrange(Species, Week)

#save as csv
write.csv(Phase1_Data_Porometer_Avg, "data_QAQC/Phase1_Data_Porometer_Avg.csv", quote = FALSE, row.names = FALSE)


################################################################################
#Graph! Porometer Data
################################################################################

#filter for porometer data
Phase1_Data_Porometer <- Phase1_Data_Porometer %>% 
  filter(PorometerSubset == "yes", !is.na(Porometer), Treatment_water == "Drought")

Phase1_Data_Porometer_PIPO <- Phase1_Data_Porometer %>% 
  filter(Species == "PIPO")

#Porometer Conductance
Phase1_Data_Porometer %>% 
  #filter(Species == "PIPO") %>% 
  ggplot(aes(x = Week,
             y = Porometer,
             color = Treatment_temp)) +
  geom_point() +
  #geom_line() +
  geom_errorbar(aes(x = Week, 
                    ymin=Phase1_Data_Porometer$Porometer - Phase1_Data_Porometer$SD, 
                    ymax=Phase1_Data_Porometer$Porometer + Phase1_Data_Porometer$SD),
               width=0.1, color='black', alpha = 0.5) +
  ylim(0, 600) +
  xlim(0,40) +
  annotate("segment",
           x = 7, xend = 7,
           y = 0, yend = 350,
           color = "red",
           linetype = "dashed",
           size = 0.4) +
  geom_text(label = "Heatwave",
            x = 12, y = 300, color = "red", size = 3) +
  facet_wrap(~CommonName) +
  xlab("Week") +
  ylab("Stomatal Conductance") +
  labs(title = "Stomatal Conductance - Ponderosa Pine") +
  theme_minimal() +
  scale_color_discrete(direction = -1)





################################################################################
#read CSVs
Phase1_Data_Porometer_Avg <- read_csv("data_QAQC/Phase1_Data_Porometer_Avg.csv")


#Porometer Conductance
Phase1_Data_Porometer_Avg %>% 
  filter(Treatment_water == "Drought") %>%
  group_by(Treatment_temp) %>%
  ggplot(aes(x = Week,
             y = Porometer,
             color = Treatment_temp)) +
  geom_point() +
  geom_line() +
  ylim(0, 350) +
  xlim(0,36) +
  annotate("segment",
           x = 7, xend = 7,
           y = 0, yend = 350,
           color = "red",
           linetype = "dashed",
           size = 0.4) +
  geom_text(label = "Heatwave",
            x = 12, y = 300, color = "red", size = 3) +
  facet_wrap(~CommonName) +
  xlab("Week") +
  ylab("Stomatal Conductance") +
  labs(title = "Stomatal Conductance of Droughted Trees") +
  theme_minimal() +
  scale_color_discrete(direction = -1)

#Porometer Temperatures
Phase1_Data_Porometer_Avg %>%
  filter(Treatment_water == "Drought") %>%
  group_by(Species) %>%
  ggplot(aes(x = Week,
             y = Temperature_C,
             color = CommonName)) +
  geom_point() +
  ylim(0, 40) +
  xlab("Week") +
  ylab("Temperature (Celcius)") +
  labs(title = "Temperature Fluctuations") +
  theme_minimal()

#Porometer Leaf Sensor RH
Phase1_Data_Porometer_Avg %>% 
  filter(Treatment_water == "Drought") %>%
  group_by(Species) %>%
  ggplot(aes(x = Week,
             y = LeafSensor_PercentRH,
             color = CommonName)) +
  geom_point() +
  ylim(0, 100) +
  xlab("Week") +
  ylab("Leaf Sensor (%RH)") +
  labs(title = "Leaf Sensor RH Fluctuations") +
  theme_minimal()

#Porometer Filter Sensor RH
Phase1_Data_Porometer_Avg %>% 
  filter(Treatment_water == "Drought") %>%
  group_by(Species) %>%
  ggplot(aes(x = Week,
             y = FilterSensor_PercentRH,
             color = CommonName)) +
  geom_point() +
  ylim(0, 40) +
  xlab("Week") +
  ylab("Filter Sensor (%RH)") +
  labs(title = "Filter Sensor RH Fluctuations") +
  theme_minimal()

