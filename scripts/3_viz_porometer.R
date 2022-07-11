#QAQC - check porometer data against plant data
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-07-07
#Last updated: 2022-07-07

#load tidyverse
library(tidyverse)

#read in csvs
Phase1_Porometer_QAQC <- read_csv("data_QAQC/Phase1_Porometer_QAQC.csv")

#filter for porometer data
Phase1_Porometer_QAQC <- Phase1_Porometer_QAQC %>% 
  filter(PorometerSubset == "yes", !is.na(Porometer))

#check out data
glimpse(Phase1_Porometer_QAQC)

#convert variables
#if needed, convert time from double <dbl> to character <chr>
Phase1_Porometer_QAQC <- Phase1_Porometer_QAQC %>% 
  mutate(Time = ifelse(Time < 1000 & Time > 30, paste0("0", Phase1_Porometer$Time), 
                       ifelse(Time == 30, paste0("00", Phase1_Porometer$Time), 
                              ifelse(Time == 0, paste0("000", Phase1_Porometer$Time), Time))))
Phase1_Porometer_QAQC$DateTime <- as.POSIXct(Phase1_Porometer_QAQC$DateTime, format = "%m/%d/%Y %H:%M")
Phase1_Porometer_QAQC$Date <- as.Date(Phase1_Porometer_QAQC$Date, format = "%m/%d/%Y")

Phase1_Porometer_QAQC$Phase <- as.factor(Phase1_Porometer_QAQC$Phase)
Phase1_Porometer_QAQC$Chamber <- as.factor(Phase1_Porometer_QAQC$Chamber)
Phase1_Porometer_QAQC$ScientificName <- as.factor(Phase1_Porometer_QAQC$ScientificName)
Phase1_Porometer_QAQC$CommonName <- as.factor(Phase1_Porometer_QAQC$CommonName)
Phase1_Porometer_QAQC$Species <- as.factor(Phase1_Porometer_QAQC$Species)
Phase1_Porometer_QAQC$Treatment_temp <- as.factor(Phase1_Porometer_QAQC$Treatment_temp)
Phase1_Porometer_QAQC$Treatment_water <- as.factor(Phase1_Porometer_QAQC$Treatment_water)
Phase1_Porometer_QAQC$PorometerSubset <- as.factor(Phase1_Porometer_QAQC$PorometerSubset)
Phase1_Porometer_QAQC$Whorls <- as.factor(Phase1_Porometer_QAQC$Whorls)
Phase1_Porometer_QAQC$PercentBrown <- as.factor(Phase1_Porometer_QAQC$PercentBrown)
Phase1_Porometer_QAQC$Dead <- as.factor(Phase1_Porometer_QAQC$Dead)

################################################################################

#summarize average data 
Phase1_Porometer_QAQC_sum <- Phase1_Porometer_QAQC %>%
  group_by(Species, Week, Treatment_temp, Treatment_water) %>% 
  summarize(Porometer = mean(Porometer),
            Temperature = mean(Temperature_C),
            LeafSensor = mean(LeafSensor_PercentRH),
            FilterSensor = mean(FilterSensor_PercentRH)) %>% 
  arrange(Species, Week)

#Look at droughted trees
Phase1_Porometer_QAQC_sum_graph <- Phase1_Porometer_QAQC_sum %>% 
  filter(Treatment_water == "Drought") 
  # filter(Species == "PIPO" | Species == "PIED")


#Graphs!
#Porometer Conductance
Phase1_Porometer_QAQC_sum_graph %>% 
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
           size = 0.8) +
  geom_text(label = "Heatwave",
            x = 12, y = 300, color = "red", size = 3) +
  facet_wrap(~Species) +
  xlab("Week") +
  ylab("Stomatal Conductance") +
  labs(title = "Stomatal Conductance of Droughted Trees") +
  theme_minimal()

#Porometer Temperatures
Phase1_Porometer_QAQC_sum_graph %>% 
  group_by(Species) %>%
  ggplot(aes(x = Week,
             y = Temperature,
             color = Species)) +
  geom_point() +
  ylim(0, 40) +
  xlab("Week") +
  ylab("Temperature (Celcius)") +
  labs(title = "Temperature Fluctuations") +
  theme_minimal()

#Porometer Leaf Sensor RH
Phase1_Porometer_QAQC_sum_graph %>% 
  group_by(Species) %>%
  ggplot(aes(x = Week,
             y = LeafSensor,
             color = Species)) +
  geom_point() +
  ylim(0, 100) +
  xlab("Week") +
  ylab("Leaf Sensor (%RH)") +
  labs(title = "Leaf Sensor RH Fluctuations") +
  theme_minimal()

#Porometer Filter Sensor RH
Phase1_Porometer_QAQC_sum_graph %>% 
  group_by(Species) %>%
  ggplot(aes(x = Week,
             y = FilterSensor,
             color = Species)) +
  geom_point() +
  ylim(0, 40) +
  xlab("Week") +
  ylab("Filter Sensor (%RH)") +
  labs(title = "Filter Sensor RH Fluctuations") +
  theme_minimal()

