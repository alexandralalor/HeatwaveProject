#Data analysis - porometer stress weeks
#find asymptote
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-09-03
#Last updated: 2022-09-03

#load packages
library(tidyverse)

#read CSVs
Data_Porometer <- read_csv("data_QAQC/Data_Porometer.csv")

#rearrange columns
#Data_Porometer <- Data_Porometer[ ,c(5,6,14,1,2,3,4,7,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)]


#check out data
#glimpse(Data_Porometer)

#convert variables
# #if needed, convert time from double <dbl> to character <chr>
# Data_Porometer <- Data_Porometer %>% 
#   mutate(Time = ifelse(Time < 1000 & Time > 30, paste0("0", Porometer$Time), 
#                        ifelse(Time == 30, paste0("00", Porometer$Time), 
#                               ifelse(Time == 0, paste0("000", Porometer$Time), Time))))
# Data_Porometer$DateTime <- as.POSIXct(Data_Porometer$DateTime, format = "%m/%d/%Y %H:%M")
# Data_Porometer$Date <- as.Date(Data_Porometer$Date, format = "%m/%d/%Y")

Data_Porometer$Phase <- as.factor(Data_Porometer$Phase)
Data_Porometer$Chamber <- as.factor(Data_Porometer$Chamber)
Data_Porometer$ScientificName <- as.factor(Data_Porometer$ScientificName)
Data_Porometer$CommonName <- as.factor(Data_Porometer$CommonName)
Data_Porometer$Species <- as.factor(Data_Porometer$Species)
Data_Porometer$Treatment_temp <- as.factor(Data_Porometer$Treatment_temp)
Data_Porometer$Treatment_water <- as.factor(Data_Porometer$Treatment_water)
Data_Porometer$PorometerSubset <- as.factor(Data_Porometer$PorometerSubset)
Data_Porometer$Dead <- as.factor(Data_Porometer$Dead)
Data_Porometer$Heatwave_graph <- as.factor(Data_Porometer$Heatwave_graph)
Data_Porometer$Heatwave <- as.factor(Data_Porometer$Heatwave)


################################################################################
# Stress Weeks
################################################################################

#new df
Data_Porometer_test <- Data_Porometer


#create stress levels (yes, no, maybe) based on porometer reading
Data_Porometer_test <- Data_Porometer_test %>% 
  mutate(Stress_Level = ifelse(Porometer_Est < 100 & Week > 1, "yes",
                               ifelse(Porometer_Est < 200 & Porometer_Est >= 100 & Week > 1, "maybe", "no")))

# identify threshold points for each stress level
Data_Porometer_test_add_1 <- Data_Porometer_test %>% 
  filter(Stress_Level == "no") %>% 
  group_by(Phase,SpeciesID) %>% 
  summarize(Stress_Week_no = max(Week))

Data_Porometer_test_add_2 <- Data_Porometer_test %>% 
  filter(Stress_Level == "yes") %>% 
  group_by(Phase,SpeciesID) %>% 
  summarize(Stress_Week_yes = min(Week))

Data_Porometer_test_add_3 <- Data_Porometer_test %>% 
  filter(Stress_Level == "maybe", Week != 20, Week != 21) %>% 
  group_by(Phase,SpeciesID) %>% 
  summarize(Stress_Week_maybe = max(Week))

Data_Porometer_test_add_4 <- merge(Data_Porometer_test_add_1, Data_Porometer_test_add_2, all = T)
Data_Porometer_test_add <- merge(Data_Porometer_test_add_3, Data_Porometer_test_add_4, all = T)
Data_Porometer_test <- merge(Data_Porometer_test, Data_Porometer_test_add, all = T)


# Refine stress weeks.
# Plant should not be stressed if future readings show 200+ porometer
# Plant might be stressed if future readings show 100-200 porometer
# Plant is stressed if future readings show less than 100 porometer
Data_Porometer_test_add_5 <- Data_Porometer_test %>% 
  filter(Treatment_water == "Drought") %>% 
  #filter(Week != 20, Week != 21) %>% 
  mutate(Stress_1 = ifelse(Stress_Week_no >= Week, "no", 
                           ifelse(Stress_Week_maybe >= Week, "maybe", "yes")))

Data_Porometer_test_add_6 <- Data_Porometer_test %>% 
  filter(Treatment_water == "Drought") %>% 
  #filter(Week == 20 | Week == 21) %>% 
  mutate(Stress_1 = "yes")

Data_Porometer_test_add_7 <- Data_Porometer_test %>% 
  filter(Treatment_water == "Watered") %>% 
  mutate(Stress_1 = ifelse(Stress_Week_no >= Week, "no", 
                           ifelse(Stress_Week_maybe >= Week, "maybe", "yes")))

Data_Porometer_test_add_8 <- merge(Data_Porometer_test_add_5, Data_Porometer_test_add_6, all = T)
Data_Porometer_test_add <- merge(Data_Porometer_test_add_7, Data_Porometer_test_add_8, all = T)
Data_Porometer_test <- merge(Data_Porometer_test, Data_Porometer_test_add, all = T)

Data_Porometer_test <- Data_Porometer_test %>% 
  mutate(Stress_2 = ifelse(is.na(Stress_1), "yes", Stress_1))


#manual transitions that I can't figure out
Data_Porometer_test <- Data_Porometer_test %>% 
  mutate(Stress_3 = ifelse(SpeciesID == "PIED33" & Week >= 5, "yes", 
                           ifelse(SpeciesID == "PIPO33" & Week >= 5, "yes", 
                                  ifelse(SpeciesID == "PIED03" & Week >= 9, "yes", Stress_2))))

#find stress point
# isolate 
Data_Porometer_test_add_9 <- Data_Porometer_test %>%
  filter(Stress_3 == "yes") %>%
  group_by(SpeciesID) %>%
  summarize(Stress_Week = min(Week))

Data_Porometer_test <- merge(Data_Porometer_test, Data_Porometer_test_add_9, all = T)


# Add to all data
Data_Porometer_add <- Data_Porometer_test %>% 
  mutate(Stress_Level = Stress_3) %>% 
  select(c("Phase","SpeciesID", "Week", "Stress_Level", "Stress_Week"))
  
Data_Porometer_1 <- merge(Data_Porometer, Data_Porometer_add, all = T)


#reorder and rearrange columns
#Data_Porometer_1 <- Data_Porometer_1[, c(3,4,5,6,7,1,8,9,10,11,12,13,14,2,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33)]
Data_Porometer <- Data_Porometer_1 %>% 
  group_by(Phase,Species) %>% 
  arrange(Phase,SpeciesID, Week)



#save as csv
write.csv(Data_Porometer, "data_analysis/Data_Porometer.csv", quote = FALSE, row.names = FALSE)





################################################################################
# validation
################################################################################

#check
# PIED---
# PIED03 (fixed)
# PIED33 (fixed)
# PIED49 (fixed)
# PIPO---
# PIPO33 (fixed)
# PIFL---
# PSME---
# PIEN---

# Data_Porometer_filter <- Data_Porometer_test %>%
#   filter(SpeciesID == "PIPO33")
#
# #Weight over time (averaged)
# Data_Porometer_filter %>%
#   filter(SpeciesID == "PIPO33") %>%
#   ggplot(aes(x = Week,
#              y = Porometer,
#              color = Stress_3)) +
#   geom_point() +
#   annotate("segment",
#            x = 7, xend = 7,
#            y = 0, yend = 600,
#            color = "red",
#            linetype = "dashed",
#            size = 0.6) +
#   annotate("segment",
#            x = Data_Porometer_filter$Stress_Week,
#            xend = Data_Porometer_filter$Stress_Week,
#            y = 0, yend = 600,
#            color = "black",
#            linetype = "dashed",
#            size = 0.6) +
#   ylim(0, 600) +
#   xlim(0,36) +
#   #facet_wrap(~CommonName) +
#   xlab("Week") +
#   ylab("Stomatal Conductance") +
#   labs(title = "Stomatal Conductance of Droughted Trees") +
#   theme_minimal()
# #theme(legend.position = "none")



