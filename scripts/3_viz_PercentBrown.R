#Data viz - percent brown
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-07-13
#Last updated: 2022-07-13

#load tidyverse
library(tidyverse)

#read CSVs
Phase1_Data_PercentBrown <- read_csv("data_QAQC/Phase1_Data_PercentBrown.csv")

#check out data
glimpse(Phase1_Data_PercentBrown)

#convert variables
Phase1_Data_PercentBrown$Phase <- as.factor(Phase1_Data_PercentBrown$Phase)
Phase1_Data_PercentBrown$Chamber <- as.factor(Phase1_Data_PercentBrown$Chamber)
Phase1_Data_PercentBrown$ScientificName <- as.factor(Phase1_Data_PercentBrown$ScientificName)
Phase1_Data_PercentBrown$CommonName <- as.factor(Phase1_Data_PercentBrown$CommonName)
Phase1_Data_PercentBrown$Species <- as.factor(Phase1_Data_PercentBrown$Species)
Phase1_Data_PercentBrown$Treatment_temp <- as.factor(Phase1_Data_PercentBrown$Treatment_temp)
Phase1_Data_PercentBrown$Treatment_water <- as.factor(Phase1_Data_PercentBrown$Treatment_water)
Phase1_Data_PercentBrown$PorometerSubset <- as.factor(Phase1_Data_PercentBrown$PorometerSubset)
Phase1_Data_PercentBrown$Dead <- as.factor(Phase1_Data_PercentBrown$Dead)
Phase1_Data_PercentBrown$Heatwave_graph <- as.factor(Phase1_Data_PercentBrown$Heatwave_graph)
Phase1_Data_PercentBrown$Heatwave <- as.factor(Phase1_Data_PercentBrown$Heatwave)


################################################################################
#Graph! Percent Brown Data
################################################################################

#read CSVs
Phase1_Data_PercentBrown_Avg <- read_csv("data_analysis/Phase1_Data_PercentBrown_Avg.csv")

unique(Phase1_Data_PercentBrown_Avg$CommonName)

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


#Percent brown over time (averaged)
Phase1_Data_PercentBrown_Avg %>% 
  filter(Treatment_water == "Drought") %>% 
  group_by(Treatment_temp) %>% 
  ggplot(aes(x = Week,
             y = PercentBrown_Est,
             color = CommonName)) +
  geom_point() +
  geom_line() +
  ylim(0, 100) +
  xlim(0,36) +
  facet_wrap(~Treatment_temp) +
  geom_text(data = HW_label,
            aes(label = Heatwave),
            x = 11, y = 95,
            color = "red",
            size = 2.8) +
  geom_rect(data = HW_rect,
            aes(xmin = 7, xmax = 8,
                ymin = 10, ymax = 100),
            fill = "red",
            color = "red",
            alpha = 0.05) +
  theme_minimal()
