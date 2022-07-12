#Data viz - weights
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-07-07
#Last updated: 2022-07-12

#load tidyverse
library(tidyverse)
library(cobs) #for conreg, concave/convex package

#read CSVs
Phase1_Data <- read_csv("data_QAQC/Phase1_Data.csv")

################################################################################
#first, add info to weight data
#WeightMin, WeightMax, WaterWeight_Base, WaterWeight_Calc, PercentWater, Weight_Est
################################################################################

#Calculate WeightMin and WeightMax for each individual plant
Phase1_Data_Weight_add_1 <- Phase1_Data %>% 
  filter(Treatment_water == "Drought") %>% 
  group_by(SpeciesID) %>% 
  mutate(WeightMin = min(Weight_g, na.rm = TRUE),
         WeightMax = max(Weight_g, na.rm = TRUE)) %>% 
  select(Species, SpeciesID, Week, WeightMin, WeightMax)

#Add data to Phase1_Data_Weight
Phase1_Data_Weight <- merge(Phase1_Data, Phase1_Data_Weight_add_1, by = c("Species","SpeciesID", "Week"), all = TRUE)

#Calculate WaterWeight_Base, Waterweight_Calc, and PercentWater for each plant
#WaterWeight_Base = how many grams of water did the plant start out with?
#WaterWeight_Calc = how many grams of water does a plant have weekly?
#PercentWater = what percent water does a plant have weekly?
# -- all assuming WeightMin is completely dry and WeightMax is at field capacity
Phase1_Data_Weight <- Phase1_Data_Weight %>% 
  mutate(WaterWeight_Base = WeightMax - WeightMin,
         WaterWeight_Calc = Weight_g - WeightMin) %>% 
  mutate(PercentWater = 100*(WaterWeight_Calc/WaterWeight_Base))

#Calculate Weight_Est
#Weight_Est = estimated weight of plants which have died and dropped out of study
#assuming WeightMin is completely dry, and as the lowest weight
Phase1_Data_Weight_add_2 <- Phase1_Data_Weight %>% 
  filter(!grepl(".5", Phase1_Data_Weight$Week, fixed = TRUE)) %>% 
  filter(Treatment_water == "Drought", is.na(PercentBrown)) %>% 
  mutate(Weight_Est = ifelse(Dead == "dead", WeightMin, Weight_g)) %>% 
  select(c("Species","SpeciesID","Week","Weight_Est"))

#Add data to Phase1_Data_Weight
Phase1_Data_Weight <- merge(Phase1_Data_Weight, Phase1_Data_Weight_add_2, by = c("Species","SpeciesID", "Week"), all = TRUE)

#Combine all weight data to include Weight_Est
Phase1_Data_Weight <- Phase1_Data_Weight %>% 
  mutate(Weight_Est = ifelse(is.na(Weight_Est), Phase1_Data_Weight$Weight_g, Phase1_Data_Weight$Weight_Est))

#Now convert to NA during missing christmas readings
Phase1_Data_Weight_testing_1 <- Phase1_Data_Weight
Phase1_Data_Weight_testing_1$Weight_Est1 <- ifelse(Phase1_Data_Weight_testing_1$Species == "PIFL" & Phase1_Data_Weight_testing_1$Week == 16, NA, Phase1_Data_Weight_testing_1$Weight_Est)
Phase1_Data_Weight <- Phase1_Data_Weight_testing_1
Phase1_Data_Weight_testing_2 <- Phase1_Data_Weight
Phase1_Data_Weight_testing_2$Weight_Est2 <- ifelse(Phase1_Data_Weight_testing_2$Species == "PSME" & Phase1_Data_Weight_testing_2$Week == 16, NA, Phase1_Data_Weight_testing_2$Weight_Est1)
Phase1_Data_Weight <- Phase1_Data_Weight_testing_2
Phase1_Data_Weight_testing_3 <- Phase1_Data_Weight
Phase1_Data_Weight_testing_3$Weight_Est <- ifelse(Phase1_Data_Weight_testing_3$Species == "PIED" & Phase1_Data_Weight_testing_3$Week == 18, NA, Phase1_Data_Weight_testing_3$Weight_Est2)
Phase1_Data_Weight <- Phase1_Data_Weight_testing_3

#last, remove extra columns
Phase1_Data_Weight <- Phase1_Data_Weight %>% 
  select(-c("Weight_Est1","Weight_Est2"))

# #check conversions
# Phase1_Data_Weight_check <- Phase1_Data_Weight %>% 
#   filter(!grepl(".5", Phase1_Data_Weight$Week, fixed = TRUE)) %>% 
#   filter(Treatment_water == "Drought") %>% 
#   group_by(Species, SpeciesID, Week, Treatment_temp, Treatment_water) %>% 
#   summarize(Dead_Count = sum(Dead_Count),
#             Weight_Est = round(mean(Weight_Est, na.rm = T), digits = 0),
#             Weight_Est1 = round(mean(Weight_Est1, na.rm = T), digits = 0),
#             Weight_Est2 = round(mean(Weight_Est2, na.rm = T), digits = 0)) %>% 
#   arrange(Species, Week, SpeciesID)

################################################################################
#Next, find average weight data for graphing
################################################################################

#first, remove 1/2 weeks, filter for drought
Phase1_Data_Weight_Avg_1 <- Phase1_Data_Weight %>% 
  filter(!grepl(".5", Phase1_Data_Weight$Week, fixed = TRUE)) %>% 
  filter(Treatment_water == "Drought") %>% 
  group_by(Species, SpeciesID, Week, Treatment_temp, Treatment_water) %>% 
  summarize(Dead_Count = sum(Dead_Count),
            Weight_Est = round(mean(Weight_Est, na.rm = T), digits = 0))
#next, remove NaN values, and find Species average
Phase1_Data_Weight_Avg <- Phase1_Data_Weight_Avg_1 %>%
  filter(!is.nan(Weight_Est)) %>%
  group_by(Species, Week, Treatment_temp, Treatment_water) %>%
  summarize(Dead_Count = sum(Dead_Count),
            Weight_Est = round(mean(Weight_Est, na.rm = T), digits = 0))

################################################################################
#Graph! Weight Data
################################################################################

#Not averaged
Phase1_Data_Weight %>% 
  filter(Treatment_water == "Drought", Species == "PIED") %>% 
  group_by(Treatment_temp) %>% 
  ggplot(aes(x = Week,
             y = Weight_g,
             color = Species)) +
  geom_point() +
  geom_point(data = Phase1_Data_Weight %>% filter(SpeciesID == "PIED07", Week < 19),
             size = 1.7,
             color = "black",
             fill = "black",
             shape = 21) +
  geom_point(data = Phase1_Data_Weight %>% filter(SpeciesID == "PIED33", Week < 15),
             size = 1.7,
             color = "black",
             fill = "black",
             shape = 21) +
  #geom_smooth(method = 'loess', se = FALSE) +
  facet_wrap(~Treatment_temp) +
  theme_minimal()

#Averaged
Phase1_Data_Weight_Avg %>% 
  group_by(Species, Treatment_temp) %>%
  ggplot(aes(x = Week,
             y = Weight_Est,
             color = Species)) +
  geom_point() +
  annotate("segment",
           x = 7, xend = 7,
           y = 200, yend = 800,
           color = "red",
           linetype = "dashed",
           size = 0.8) +
  geom_text(label = "Heatwave",
            x = 9, y = 800, color = "red", size = 3) +
  #geom_smooth(method = 'loess', se = FALSE) +
  facet_wrap(~Treatment_temp) +
  theme_minimal()

################################################################################
#Find point of inflection?
################################################################################

#calculate local regression
# ls <- Phase1_Data_Weight %>% 
#   group_by(SpeciesID, Week) %>% 
#   loess(Weight_g~Week)
# pr.loess <- predict(ls)

# plot(cars)
# x = cars$speed
# y = cars$dist
# rc = conreg(x,y,convex=TRUE)
# lines(rc, col = 2)

# #find convex point
# rc <- Phase1_Data_Weight %>%
#   group_by(SpeciesID, Week) %>%
#   filter(!is.na(Weight_Est))
# RC <- conreg(rc$Week, rc$Weight_Est, convex = TRUE)
# lines(rc, col = 2)

################################################################################
#
################################################################################


# Phase1_Data_Weight_avg <- Phase1_Data_Weight %>% 
#   filter(Treatment_water == "Drought", Dead == "dead", !is.na(Weight_g)) %>% 
#   group_by(Species, Week, Dead) %>% 
#   summarize(Weight = mean(Weight_g),
#             Dead = sum(No.Dead)) %>% 
#   filter(Dead > 1, Week > 7) %>% 
#   group_by(Species) %>% 
#   summarize(AvgDryWeight = round(mean(Weight), digits = 0))


Phase1_Data_Weight_graph_sum <- Phase1_Data_Weight_graph %>% 
  filter(Dead == "dead") %>% 
  group_by(Species, Week, Dead) %>% 
  summarize(Dead = sum(No.Dead),
            AvgDeadWeight = mean(Weight_g),
            AvgDeadPercentWater = mean(PercentWater)) %>% 
  filter(Week > 7, Dead > 1) %>% 
  group_by(Species) %>% 
  summarize(AvgDeadWeight = mean(AvgDeadWeight),
            AvgDeadPercentWater = mean(AvgDeadPercentWater))
            

Phase1_Data_Weight %>% 
  group_by(Species) %>% 
  filter(Dead == "dead", Week > 7) %>% 
  ggplot(aes(x = Species,
             y = PercentWater,
             color = Species)) +
  geom_boxplot()

Phase1_Data_Weight %>% 
  group_by(Species, Week) %>%
  filter(Species == "PIED") %>% 
  ggplot(aes(x = Week,
             y = Weight_g,
             color = Species)) +
  geom_point()

Phase1_Data_Weight_graph_1 <- Phase1_Data_Weight_graph %>% 
  group_by(Species, Week, Treatment_temp, Treatment_water) %>% 
  summarize(Weight_g = mean(Weight_g),
            DryWeight = mean(DryWeight),
            WetWeight = mean(WetWeight),
            WaterWeight_Baseline = mean(WaterWeight_Baseline),
            WaterWeight_Calc = mean(WaterWeight_Calc),
            PercentWater = mean(PercentWater),
            Dead = sum(Dead),
            PercentDead = 100*(Dead/20))

Phase1_Data_Weight_graph_1 %>% 
  filter(Dead > 0) %>% 
  ggplot(aes(x = PercentDead,
             y = PercentWater,
             color = Species)) +
  geom_point() +
  xlim(0,100) +
  ylim(0,100) +
  theme_minimal()

#Graphs!
#Weight
Phase1_Data_Weight_graph_1 %>% 
  group_by(Species) %>%
  #filter(Species == "PIPO") %>% 
  ggplot(aes(x = Week,
             y = Weight_g,
             color = Species)) +
  geom_point() +
  geom_line() +
  # geom_errorbar(aes(x = Week, ymin=Weight_g-SD, ymax=Weight_g+SD),
  #              width=0.1, color='black', alpha = 0.5) +
  ylim(200, 800) +
  xlim(0,36) +
  annotate("segment",
           x = 7, xend = 7,
           y = 200, yend = 800,
           color = "red",
           linetype = "dashed",
           size = 0.8) +
  # annotate("segment",
  #          x = 0, xend = 36,
  #          y = 386, yend = 386,
  #          color = "black",
  #          linetype = "dashed",
  #          size = 0.8) +
  geom_text(label = "Heatwave",
            x = 9, y = 800, color = "red", size = 3) +
  facet_wrap(~Treatment_temp) +
  xlab("Week") +
  ylab("Weight (g)") +
  labs(title = "Weight of Droughted Trees") +
  theme_minimal()


################################################################################

# #test to replace NaN values
# Phase1_Data_Weight_add_3 <- Phase1_Data_Weight_Avg_1 %>% 
#   filter(is.nan(Weight_Est), Dead_Count > 0) %>% 
#   select(c("Species","SpeciesID","Week","Weight_Est"))
# 
# Phase1_Data_Weight_add_4 <- Phase1_Data_Weight_add_2 %>%
#   filter(!is.na(Weight_Est)) %>% 
#   group_by(Species, SpeciesID) %>% 
#   summarise(Weight_Sum = mean(Weight_Est))
# 
# Phase1_Data_Weight_add_5 <- merge(Phase1_Data_Weight_add_2, Phase1_Data_Weight_add_3, by = c("Species","SpeciesID","Week","Weight_Est"), all = TRUE)
# Phase1_Data_Weight_add_5 <- merge(Phase1_Data_Weight_add_5, Phase1_Data_Weight_add_4, by = c("Species","SpeciesID"), all = TRUE)
# Phase1_Data_Weight_add_5 <- Phase1_Data_Weight_add_5 %>% 
#   select(c("Species","SpeciesID","Week","Weight_Sum"))
# 
# Phase1_Data_Weight_test <- merge(Phase1_Data_Weight, Phase1_Data_Weight_add_5, by = c("Species","SpeciesID", "Week"), all = TRUE)
# Phase1_Data_Weight_test <- Phase1_Data_Weight_test %>% 
#   mutate(Weight_Estimate = ifelse(is.na(Weight_Sum), Weight_Est, Weight_Sum))
# 
# 
# Phase1_Data_Weight_Avg_test <- Phase1_Data_Weight_test %>% 
#   filter(!grepl(".5", Phase1_Data_Weight$Week, fixed = TRUE)) %>% 
#   filter(Treatment_water == "Drought") %>% 
#   group_by(Species, Week, Treatment_temp, Treatment_water) %>% 
#   summarize(Dead_Count = sum(Dead_Count),
#             Weight_Estimate = round(mean(Weight_Estimate, na.rm = T), digits = 0))

