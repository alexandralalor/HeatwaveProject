#Data viz - weights
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-07-07
#Last updated: 2022-07-13

#load tidyverse
library(tidyverse)
#library(cobs) #for conreg, concave/convex package

#read CSVs
Phase1_Data_Weight <- read_csv("data_QAQC/Phase1_Data_Weight.csv")

#check out data
glimpse(Phase1_Data_Weight)

#convert variables
Phase1_Data_Weight$Phase <- as.factor(Phase1_Data_Weight$Phase)
Phase1_Data_Weight$Chamber <- as.factor(Phase1_Data_Weight$Chamber)
Phase1_Data_Weight$ScientificName <- as.factor(Phase1_Data_Weight$ScientificName)
Phase1_Data_Weight$CommonName <- as.factor(Phase1_Data_Weight$CommonName)
Phase1_Data_Weight$Species <- as.factor(Phase1_Data_Weight$Species)
Phase1_Data_Weight$Treatment_temp <- as.factor(Phase1_Data_Weight$Treatment_temp)
Phase1_Data_Weight$Treatment_water <- as.factor(Phase1_Data_Weight$Treatment_water)
Phase1_Data_Weight$PorometerSubset <- as.factor(Phase1_Data_Weight$PorometerSubset)
Phase1_Data_Weight$Dead <- as.factor(Phase1_Data_Weight$Dead)
Phase1_Data_Weight$Dead_Count <- as.factor(Phase1_Data_Weight$Dead_Count)
Phase1_Data_Weight$Heatwave_graph <- as.factor(Phase1_Data_Weight$Heatwave_graph)
Phase1_Data_Weight$Heatwave <- as.factor(Phase1_Data_Weight$Heatwave)


################################################################################
#Find average weight data for graphing
################################################################################

#filter for weight data
Phase1_Data_Weight <- Phase1_Data_Weight %>% 
  filter(!grepl(".5", Phase1_Data_Weight$Week, fixed = TRUE), !is.nan(Weight_Est))

#average data 
Phase1_Data_Weight_Avg <- Phase1_Data_Weight %>%
  group_by(ScientificName, CommonName, Species, Week, Treatment_temp, Treatment_water) %>%
  summarize(Dead_Count = sum(Dead_Count),
            PercentDead = 100*(Dead_Count/20),
            Weight_Est = round(mean(Weight_Est, na.rm = T), digits = 0),
            WeightMin = round(mean(WeightMin, na.rm = T), digits = 0),
            WeightMax = round(mean(WeightMax, na.rm = T), digits = 0),
            WaterWeight_Base = round(mean(WaterWeight_Base, na.rm = T), digits = 0),
            WaterWeight_Calc = round(mean(WaterWeight_Calc, na.rm = T), digits = 0),
            PercentWater = round(mean(PercentWater, na.rm = T), digits = 0)) %>% 
  arrange(Species, Week)

#save as csv
write.csv(Phase1_Data_Weight_Avg, "data_QAQC/Phase1_Data_Weight_Avg.csv", quote = FALSE, row.names = FALSE)


################################################################################
#Graph! Weight Data
################################################################################

#read CSVs
Phase1_Data_Weight_Avg <- read_csv("data_QAQC/Phase1_Data_Weight_Avg.csv")
Phase1_Data_Weight <- read_csv("data_QAQC/Phase1_Data_Weight.csv")

# A data frame with labels for each facet
HW_label <- data.frame(CommonName = c("Ponderosa Pine","Pinyon Pine","Limber Pine","Douglas fir","Engelman Spruce"),
                       Species = c("PIPO","PIED","PIFL","PSME","PIEN"),
                       Treatment_temp = "Ambient_HW",
                       Heatwave = "Heatwave")

HW_rect <- data.frame(CommonName = c("Ponderosa Pine","Pinyon Pine","Limber Pine","Douglas fir","Engelman Spruce"),
                      Species = c("PIPO","PIED","PIFL","PSME","PIEN"),
                      Treatment_temp = "Ambient_HW",
                      Week = 7,
                      PercentBrown_Est = 100,
                      Weight_Est = 100,
                      WaterWeight_Calc = 100,
                      PercentWater = 100)


#Weight over time (averaged)
Phase1_Data_Weight %>% 
  #filter(Treatment_water == "Drought") %>%
  group_by(SpeciesID) %>%
  filter(Species == "PIED") %>% 
  ggplot(aes(x = Week,
             y = Weight_g,
             color = Treatment_water)) +
  geom_point() +
  geom_line() +
  # geom_errorbar(aes(x = Week, ymin=Weight_Est-SD, ymax=Weight_Est+SD),
  #              width=0.1, color='black', alpha = 0.5) +
  ylim(200, 800) +
  xlim(0,36) +
  geom_text(data = HW_label,
            aes(label = Heatwave),
            x = 11, y = 800,
            color = "red",
            size = 2.8) +
  # geom_rect(data = HW_rect,
  #           aes(xmin = 7, xmax = 8,
  #               ymin = 200, ymax = 800),
  #           fill = "red",
  #           color = "red",
  #           alpha = 0.05) +
  annotate("segment",
           x = 7, xend = 7,
           y = 200, yend = 800,
           color = "red",
           linetype = "dashed",
           size = 0.6) +
  facet_wrap(~Treatment_temp) +
  xlab("Week") +
  ylab("Water Weight (g)") +
  labs(title = "Weight of Droughted Trees") +
  theme_minimal()


#Water weight over time (averaged)
Phase1_Data_Weight_Avg %>% 
  filter(Treatment_water == "Drought", Species == "PSME") %>%
  group_by(Species) %>%
  ggplot(aes(x = Week)) +
  geom_point(aes(y = WaterWeight_Calc),
             color = "blue") +
  geom_line(aes(y = WaterWeight_Calc),
            color = "blue") +
  geom_point(aes(y = PercentDead * scaleFactor_2),
             color = "red") +
  geom_line(aes(y = PercentDead * scaleFactor_2),
            color = "red") +
  scale_y_continuous(name="WaterWeight_Calc", sec.axis=sec_axis(~./scaleFactor_2, name="PercentDead")) +
  annotate("segment",
         x = 7, xend = 7,
         y = 0, yend = 400,
         color = "red",
         linetype = "dashed",
         size = 0.6) +
  facet_wrap(~Treatment_temp) +
  xlab("Week") +
  ylab("Water Weight (g)") +
  labs(title = "Water Weight of Droughted Trees") +
  theme_minimal()


#Water weight over time (averaged)
Phase1_Data_Weight_Avg %>% 
  filter(Treatment_water == "Drought") %>%
  group_by(Species) %>%
  ggplot(aes(x = Week,
             y = WaterWeight_Calc,
             color = CommonName)) +
  geom_point() +
  geom_line() +
  # geom_errorbar(aes(x = Week, ymin=Weight_Est-SD, ymax=Weight_Est+SD),
  #              width=0.1, color='black', alpha = 0.5) +
  ylim(0, 400) +
  xlim(0,36) +
  # geom_text(data = HW_label,
  #           aes(label = Heatwave),
  #           x = 11, y = 400,
  #           color = "red",
  #           size = 2.8) +
  # geom_rect(data = HW_rect,
  #           aes(xmin = 7, xmax = 8,
  #               ymin = 0, ymax = 400),
  #           fill = "red",
  #           color = "red",
  #           alpha = 0.05) +
  annotate("segment",
           x = 7, xend = 7,
           y = 0, yend = 400,
           color = "red",
           linetype = "dashed",
           size = 0.6) +
  facet_wrap(~Treatment_temp) +
  xlab("Week") +
  ylab("Water Weight (g)") +
  labs(title = "Water Weight of Droughted Trees") +
  theme_minimal()


#Percent water over time (averaged)
Phase1_Data_Weight_Avg %>% 
  filter(Treatment_water == "Drought", Treatment_temp == "Ambient_HW") %>%
  group_by(Species) %>%
  ggplot(aes(x = Week)) +
  geom_point(aes(y = PercentWater),
             color = "blue") +
  geom_line(aes(y = PercentWater),
            color = "blue") +
  geom_point(aes(y = PercentDead * scaleFactor_3),
             color = "red") +
  geom_line(aes(y = PercentDead * scaleFactor_3),
            color = "red") +
  scale_y_continuous(name="PercentWater", sec.axis=sec_axis(~./scaleFactor_3, name="PercentDead")) +
  # geom_errorbar(aes(x = Week, ymin=Weight_Est-SD, ymax=Weight_Est+SD),
  #              width=0.1, color='black', alpha = 0.5) +
  ylim(0, 100) +
  xlim(0,36) +
  annotate("segment",
         x = 7, xend = 7,
         y = 0, yend = 100,
         color = "red",
         linetype = "dashed",
         size = 0.6) +
  facet_wrap(~Species) +
  xlab("Week") +
  ylab("Percent Water") +
  labs(title = "Percent Water of Droughted Trees") +
  theme_minimal()

#Percent water over time (averaged)
Phase1_Data_Weight_Avg %>% 
  filter(Treatment_water == "Drought") %>%
  group_by(Species) %>%
  ggplot(aes(x = Week,
             y = PercentWater,
             color = CommonName)) +
  geom_point() +
  geom_line() +
  # geom_errorbar(aes(x = Week, ymin=Weight_Est-SD, ymax=Weight_Est+SD),
  #              width=0.1, color='black', alpha = 0.5) +
  ylim(0, 100) +
  xlim(0,36) +
  # geom_text(data = HW_label,
  #           aes(label = Heatwave),
  #           x = 11, y = 100,
  #           color = "red",
  #           size = 2.8) +
  # geom_rect(data = HW_rect,
  #           aes(xmin = 7, xmax = 8,
  #               ymin = 0, ymax = 100),
  #           fill = "red",
  #           color = "red",
  #           alpha = 0.05) +
  annotate("segment",
           x = 7, xend = 7,
           y = 0, yend = 100,
           color = "red",
           linetype = "dashed",
           size = 0.6) +
  facet_wrap(~Treatment_temp) +
  xlab("Week") +
  ylab("Percent Water") +
  labs(title = "Percent Water of Droughted Trees") +
  theme_minimal()


#Water weight over time, comparing heatwave
#add line when %Dead > 50%, or second line graph with % dead
Phase1_Data_Weight_Avg %>% 
  filter(Treatment_water == "Drought") %>%
  group_by(Species, Treatment_temp) %>%
  ggplot(aes(x = Week,
             y = WaterWeight_Calc,
             color = Treatment_temp)) +
  geom_point(alpha = 0.5) +
  geom_line() +
  # geom_errorbar(aes(x = Week, ymin=Weight_Est-SD, ymax=Weight_Est+SD),
  #              width=0.1, color='black', alpha = 0.5) +
  ylim(0, 400) +
  xlim(0,36) +
  annotate("segment",
           x = 7, xend = 7,
           y = 0, yend = 400,
           color = "red",
           linetype = "dashed",
           size = 0.6) +
  geom_text(label = "Heatwave",
            x = 13, y = 400, color = "red", size = 2.5) +
  facet_wrap(~CommonName) +
  xlab("Week") +
  ylab("Water Weight (g)") +
  labs(title = "Water Weight of Droughted Trees") +
  theme_minimal() +
  scale_color_discrete(direction = -1)

#Percent water over time, comparing heatwave
#add line when %Dead > 50%, or second line graph with % dead
Phase1_Data_Weight_Avg %>% 
  filter(Treatment_water == "Drought") %>%
  group_by(Species, Treatment_temp) %>%
  ggplot(aes(x = Week,
             y = PercentWater,
             color = Treatment_temp)) +
  geom_point(alpha = 0.5) +
  geom_line() +
  # geom_errorbar(aes(x = Week, ymin=Weight_Est-SD, ymax=Weight_Est+SD),
  #              width=0.1, color='black', alpha = 0.5) +
  ylim(0, 100) +
  xlim(0,36) +
  annotate("segment",
           x = 7, xend = 7,
           y = 0, yend = 100,
           color = "red",
           linetype = "dashed",
           size = 0.6) +
  geom_text(label = "Heatwave",
            x = 13, y = 100, color = "red", size = 2.5) +
  facet_wrap(~CommonName) +
  xlab("Week") +
  ylab("Water Weight (g)") +
  labs(title = "Percent Water of Droughted Trees") +
  theme_minimal() +
  scale_color_discrete(direction = -1)


################################################################################
#not sure how useful these are, but other graph ideas

#Weight over time (not averaged)
Phase1_Data_Weight %>% 
  filter(Treatment_water == "Drought", Species == "PIED") %>% 
  group_by(Treatment_temp) %>% 
  ggplot(aes(x = Week,
             y = Weight_Est,
             color = Species)) +
  geom_point() +
  geom_point(data = Phase1_Data_Weight %>% filter(SpeciesID == "PIED07"),
             size = 1.7,
             color = "black",
             fill = "black",
             shape = 21) +
  geom_point(data = Phase1_Data_Weight %>% filter(SpeciesID == "PIED33"),
             size = 1.7,
             color = "black",
             fill = "black",
             shape = 21) +
  #geom_smooth(method = 'loess', se = FALSE) +
  facet_wrap(~Treatment_temp) +
  theme_minimal()

#boxplot
Phase1_Data_Weight %>% 
  filter(Dead == "dead", Treatment_water == "Drought") %>% 
  group_by(Species) %>% 
  ggplot(aes(x = Species,
             y = PercentWater,
             color = CommonName)) +
  geom_boxplot()

#PercentDead vs PercentWater
Phase1_Data_Weight_Avg %>% 
  filter(Dead_Count > 0, Treatment_water == "Drought") %>% 
  ggplot(aes(x = PercentDead,
             y = PercentWater,
             color = CommonName)) +
  geom_point() +
  xlim(0,100) +
  ylim(0,100) +
  theme_minimal()



#######################



#scale factor for 2 y-axes
scaleFactor_1 <- max(Phase1_Data_Weight_Avg$Weight_Est, na.rm = T) / max(Phase1_Data_Weight_Avg$PercentDead, na.rm = T)
scaleFactor_2 <- max(Phase1_Data_Weight_Avg$WaterWeight_Calc, na.rm = T) / max(Phase1_Data_Weight_Avg$PercentDead, na.rm = T)
scaleFactor_3 <- max(Phase1_Data_Weight_Avg$PercentWater, na.rm = T) / max(Phase1_Data_Weight_Avg$PercentDead, na.rm = T)


#Weight + Porometer oover time
Phase1_Data_Weight_Avg %>% 
  filter(Treatment_water == "Drought", Species == "PSME") %>%
  group_by(Species) %>%
  ggplot(aes(x = Week)) +
  geom_point(aes(y = Weight_Est),
             color = "blue") +
  geom_line(aes(y = Weight_Est),
            color = "blue") +
  geom_point(aes(y = PercentDead * scaleFactor_1),
             color = "red") +
  geom_line(aes(y = PercentDead * scaleFactor_1),
            color = "red") +
  scale_y_continuous(name="Weight_Est", sec.axis=sec_axis(~./scaleFactor_1, name="PercentDead")) +
  # geom_errorbar(aes(x = Week, ymin=Weight_Est-SD, ymax=Weight_Est+SD),
  #              width=0.1, color='black', alpha = 0.5) +
  # ylim(0, 800) +
  # xlim(0,36) +
  # geom_text(data = HW_label,
  #           aes(label = Heatwave),
  #           x = 11, y = 800,
  #           color = "red",
  #           size = 2.8) +
  # geom_rect(data = HW_rect,
  #           aes(xmin = 7, xmax = 8,
#               ymin = 0, ymax = 800),
#           fill = "red",
#           color = "red",
#           alpha = 0.05) +
annotate("segment",
         x = 7, xend = 7,
         y = 0, yend = 800,
         color = "red",
         linetype = "dashed",
         size = 0.6) +
  facet_wrap(~Treatment_temp) +
  xlab("Week") +
  ylab("Total Weight (g)") +
  labs(title = "Weight of Droughted Trees") +
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
#Additional tests
################################################################################

# Phase1_Data_Weight_Avg_test <- Phase1_Data_Weight %>%
#   filter(Treatment_water == "Drought", Dead == "dead", !is.na(Weight_Est)) %>%
#   group_by(Species, Week, Dead) %>%
#   summarize(Weight_Est = mean(Weight_Est),
#             Dead_Count = sum(Dead_Count)) %>%
#   filter(Dead_Count > 1, Week > 7) %>%
#   group_by(Species) %>%
#   summarize(AvgDryWeight = round(mean(Weight_Est), digits = 0))
# 
# Phase1_Data_Weight_Avg_Sum_test <- Phase1_Data_Weight %>% 
#   filter(Dead == "dead") %>% 
#   group_by(Species, Week, Dead) %>% 
#   summarize(Dead_Count = sum(Dead_Count),
#             AvgDeadWeight = mean(Weight_Est),
#             AvgDeadPercentWater = mean(PercentWater)) %>% 
#   filter(Week > 7, Dead > 1) %>% 
#   group_by(Species) %>% 
#   summarize(AvgDeadWeight = mean(AvgDeadWeight, na.rm = T),
#             AvgDeadPercentWater = mean(AvgDeadPercentWater, na.rm = T))


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

