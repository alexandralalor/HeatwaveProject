#Data analysis - anova
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-10-14
#Last updated: 2022-10-14

#load packages
library(tidyverse)

#read csvs
heatwave <- read.csv("data_analysis/Dead_Week.csv")
#heatwave <- read.csv("data_analysis/Dead_Week_Weight.csv")
heatwave <- read.csv("data_analysis/Dead_Week_Porometer.csv")

heatwave <- heatwave %>% 
  mutate(Dead_Week = Stress_to_Dead_Porometer)


#PIPO
amb <- heatwave %>%
  filter(Species == "PIPO", Treatment_temp == "Ambient") %>% 
  mutate(Ambient = Dead_Week) %>% 
  select("Ambient")
hw <- heatwave %>%
  filter(Species == "PIPO", Treatment_temp == "Ambient_HW") %>% 
  mutate(Heatwave = Dead_Week) %>% 
  select("Heatwave")
PIPO <- cbind(amb, hw)
PIPO <- PIPO %>% 
  mutate(Species = "PIPO")

#PIED
amb <- heatwave %>%
  filter(Species == "PIED", Treatment_temp == "Ambient") %>% 
  mutate(Ambient = Dead_Week) %>% 
  select("Ambient")
hw <- heatwave %>%
  filter(Species == "PIED", Treatment_temp == "Ambient_HW") %>% 
  mutate(Heatwave = Dead_Week) %>% 
  select("Heatwave")
PIED <- merge(amb, hw, by = 0, all.y = TRUE)
PIED <- PIED %>% 
  select("Ambient","Heatwave")
PIED <- PIED %>% 
  mutate(Species = "PIED")

#PIFL
amb <- heatwave %>%
  filter(Species == "PIFL", Treatment_temp == "Ambient") %>% 
  mutate(Ambient = Dead_Week) %>% 
  select("Ambient")
hw <- heatwave %>%
  filter(Species == "PIFL", Treatment_temp == "Ambient_HW") %>% 
  mutate(Heatwave = Dead_Week) %>% 
  select("Heatwave")
PIFL <- cbind(amb, hw)
PIFL <- PIFL %>% 
  mutate(Species = "PIFL")

#PSME
amb <- heatwave %>%
  filter(Species == "PSME", Treatment_temp == "Ambient") %>% 
  mutate(Ambient = Dead_Week) %>% 
  select("Ambient")
hw <- heatwave %>%
  filter(Species == "PSME", Treatment_temp == "Ambient_HW") %>% 
  mutate(Heatwave = Dead_Week) %>% 
  select("Heatwave")
PSME <- cbind(amb, hw)
PSME <- PSME %>% 
  mutate(Species = "PSME")

#PIEN
amb <- heatwave %>%
  filter(Species == "PIEN", Treatment_temp == "Ambient") %>% 
  mutate(Ambient = Dead_Week) %>% 
  select("Ambient")
hw <- heatwave %>%
  filter(Species == "PIEN", Treatment_temp == "Ambient_HW") %>% 
  mutate(Heatwave = Dead_Week) %>% 
  select("Heatwave")
PIEN <- cbind(amb, hw)
PIEN <- PIEN %>% 
  mutate(Species = "PIEN")

#Combine all
#anova_Dead_Week <- rbind(PIPO, PIED, PIFL, PSME, PIEN)
#anova_Dead_Week_Weight <- rbind(PIPO, PIED, PIFL, PSME, PIEN)
anova_Dead_Week_Porometer <- rbind(PIPO, PIED, PIFL, PSME, PIEN)

#save csv
# write.csv(anova_Dead_Week, "data_analysis/anova_Dead_Week.csv", quote=FALSE, row.names = FALSE)
# write.csv(anova_Dead_Week_Weight, "data_analysis/anova_Dead_Week_Weight.csv", quote = FALSE, row.names = FALSE)
write.csv(anova_Dead_Week_Porometer, "data_analysis/anova_Dead_Week_Porometer.csv", quote = FALSE, row.names = FALSE)



################################################################################
# ANOVA
################################################################################
options(digits = 5, show.signif.stars = FALSE)

#read csvs 
anova_Dead_Week <- read_csv("data_analysis/anova_Dead_Week.csv")
anova_Dead_Week_Weight <- read_csv("data_analysis/anova_Dead_Week_Weight.csv")
anova_Dead_Week_Porometer <- read_csv("data_analysis/anova_Dead_Week_Porometer.csv")

#weeks from start (anova_Dead_Week)
stripchart(Ambient ~ Species, data = anova_Dead_Week, ylab = 'Dead_Week', pch = 1, col='blue')
aggregate(anova_Dead_Week$Ambient ~ anova_Dead_Week$Species, 
          FUN = function(x) c(n = length(x), mean = mean(x), sd = sd(x))) # group n, mean, and SD


#ANOVA for ambient species
Dead_Week.aov <- aov(anova_Dead_Week$Ambient ~ anova_Dead_Week$Species, data = anova_Dead_Week)
summary(Dead_Week.aov)    # summary() produces full ANOVA table

#post-hoc multiple comparisons of ALL means ### 
# TukeyHSD() wants an object that holds a fitted model
Dead_Week_HSD <- TukeyHSD(Dead_Week.aov) # Tukey's Honest Significant Differences (HSD)
Dead_Week_HSD_results <- as.data.frame(Dead_Week_HSD$`anova_Dead_Week$Species`)

#library(agricolae)
#Dead_Week_HSD <- HSD.test(Dead_Week.aov, trt = "anova_Dead_Week$Species")

#ANOVA for heatwave species
Dead_Week_HW.aov <- aov(anova_Dead_Week$Heatwave ~ anova_Dead_Week$Species, data = anova_Dead_Week)
summary(Dead_Week_HW.aov)
#post-hoc multiple comparisons of ALL means ### 
Dead_Week_HW_HSD <- TukeyHSD(Dead_Week_HW.aov) # Tukey's Honest Significant Differences (HSD)
Dead_Week_HW_HSD_results <- as.data.frame(Dead_Week_HW_HSD$`anova_Dead_Week$Species`)

#save csv
write.csv(Dead_Week_HSD_results, "data_analysis/Dead_Week_HSD_results.csv", quote=FALSE, row.names = TRUE)
write.csv(Dead_Week_HW_HSD_results, "data_analysis/Dead_Week_HW_HSD_results.csv", quote = FALSE, row.names = TRUE)


#################################################################################################
#weeks from start of water stress (anova_Dead_Week_Weight)
stripchart(Ambient ~ Species, data = anova_Dead_Week_Weight, ylab = 'Dead_Week', pch = 1, col='blue')
aggregate(anova_Dead_Week_Weight$Ambient ~ anova_Dead_Week_Weight$Species, 
          FUN = function(x) c(n = length(x), mean = mean(x), sd = sd(x))) # group n, mean, and SD

Dead_Week_Weight.aov <- aov(anova_Dead_Week_Weight$Ambient ~ anova_Dead_Week_Weight$Species, data = anova_Dead_Week_Weight)
summary(Dead_Week_Weight.aov)    # summary() produces full ANOVA table

#post-hoc multiple comparisons of ALL means ### 
# TukeyHSD() wants an object that holds a fitted model
Dead_Week_Weight_HSD <- TukeyHSD(Dead_Week_Weight.aov) # Tukey's Honest Significant Differences (HSD)
Dead_Week_Weight_HSD
Dead_Week_Weight_HSD_results <- as.data.frame(Dead_Week_Weight_HSD$`anova_Dead_Week_Weight$Species`)

#ANOVA for heatwave species
Dead_Week_Weight_HW.aov <- aov(anova_Dead_Week_Weight$Heatwave ~ anova_Dead_Week_Weight$Species, data = anova_Dead_Week_Weight)
summary(Dead_Week_Weight_HW.aov)
#post-hoc multiple comparisons of ALL means ### 
Dead_Week_Weight_HW_HSD <- TukeyHSD(Dead_Week_Weight_HW.aov) # Tukey's Honest Significant Differences (HSD)
Dead_Week_Weight_HW_HSD
Dead_Week_Weight_HW_HSD_results <- as.data.frame(Dead_Week_Weight_HW_HSD$`anova_Dead_Week_Weight$Species`)

#save csv
write.csv(Dead_Week_Weight_HSD_results, "data_analysis/Dead_Week_Weight_HSD_results.csv", quote=FALSE, row.names = TRUE)
write.csv(Dead_Week_Weight_HW_HSD_results, "data_analysis/Dead_Week_Weight_HW_HSD_results.csv", quote = FALSE, row.names = TRUE)


################################################################################################
#weeks from start of conductance stress (anova_Dead_Week_Porometer)
stripchart(Ambient ~ Species, data = anova_Dead_Week_Porometer, ylab = 'Dead_Week', pch = 1, col='blue')
aggregate(anova_Dead_Week_Porometer$Ambient ~ anova_Dead_Week_Porometer$Species, 
          FUN = function(x) c(n = length(x), mean = mean(x), sd = sd(x))) # group n, mean, and SD

Dead_Week_Porometer.aov <- aov(anova_Dead_Week_Porometer$Ambient ~ anova_Dead_Week_Porometer$Species, data = anova_Dead_Week_Porometer)
summary(Dead_Week_Porometer.aov)    # summary() produces full ANOVA table

#post-hoc multiple comparisons of ALL means ### 
# TukeyHSD() wants an object that holds a fitted model
Dead_Week_Porometer_HSD <- TukeyHSD(Dead_Week_Porometer.aov) # Tukey's Honest Significant Differences (HSD)
Dead_Week_Porometer_HSD
Dead_Week_Porometer_HSD_results <- as.data.frame(Dead_Week_Porometer_HSD$`anova_Dead_Week_Porometer$Species`)

#ANOVA for heatwave species
Dead_Week_Porometer_HW.aov <- aov(anova_Dead_Week_Porometer$Heatwave ~ anova_Dead_Week_Porometer$Species, data = anova_Dead_Week_Porometer)
summary(Dead_Week_Porometer_HW.aov)
#post-hoc multiple comparisons of ALL means ### 
Dead_Week_Porometer_HW_HSD <- TukeyHSD(Dead_Week_Porometer_HW.aov) # Tukey's Honest Significant Differences (HSD)
Dead_Week_Porometer_HW_HSD
Dead_Week_Porometer_HW_HSD_results <- as.data.frame(Dead_Week_Porometer_HW_HSD$`anova_Dead_Week_Porometer$Species`)

#save csv
write.csv(Dead_Week_Porometer_HSD_results, "data_analysis/Dead_Week_Porometer_HSD_results.csv", quote=FALSE, row.names = TRUE)
write.csv(Dead_Week_Porometer_HW_HSD_results, "data_analysis/Dead_Week_Porometer_HW_HSD_results.csv", quote = FALSE, row.names = TRUE)

