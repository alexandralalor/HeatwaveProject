#Data analysis - anova
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-10-14
#Last updated: 2026-03-01

#load packages
library(tidyverse)
library(agricolae)

#read csvs
heatwave <- read.csv("data/data_analysis/Dead_Week.csv")
#heatwave <- read.csv("data/data_analysis/Dead_Week_Weight.csv")
#heatwave <- read.csv("data/data_analysis/Dead_Week_Porometer.csv")

# heatwave <- heatwave %>% 
#   mutate(Dead_Week = Stress_to_Dead_Porometer)

###########
# PHASE 1 #
###########

#PIPO
amb <- heatwave %>%
  filter(Species == "PIPO", Treatment_temp == "Ambient") %>% 
  mutate(Baseline = Dead_Week) %>% 
  select("Baseline")
amb.hw <- heatwave %>%
  filter(Species == "PIPO", Treatment_temp == "Ambient_HW") %>% 
  mutate(Heatwave = Dead_Week) %>% 
  select("Heatwave")
PIPO <- cbind(amb, amb.hw)
PIPO <- PIPO %>% 
  mutate(Phase = 1,
         Species = "PIPO")

#PIED
amb <- heatwave %>%
  filter(Species == "PIED", Treatment_temp == "Ambient") %>% 
  mutate(Baseline = Dead_Week) %>% 
  select("Baseline")
amb.hw <- heatwave %>%
  filter(Species == "PIED", Treatment_temp == "Ambient_HW") %>% 
  mutate(Heatwave = Dead_Week) %>% 
  select("Heatwave")
PIED <- merge(amb, amb.hw, by = 0, all.y = TRUE)
PIED <- PIED %>% 
  select("Baseline","Heatwave")
PIED <- PIED %>% 
  mutate(Phase = 1,
         Species = "PIED")

#PIFL
amb <- heatwave %>%
  filter(Species == "PIFL", Treatment_temp == "Ambient") %>% 
  mutate(Baseline = Dead_Week) %>% 
  select("Baseline")
amb.hw <- heatwave %>%
  filter(Species == "PIFL", Treatment_temp == "Ambient_HW") %>% 
  mutate(Heatwave = Dead_Week) %>% 
  select("Heatwave")
PIFL <- cbind(amb, amb.hw)
PIFL <- PIFL %>% 
  mutate(Phase = 1,
         Species = "PIFL")

#PSME
amb <- heatwave %>%
  filter(Species == "PSME", Treatment_temp == "Ambient") %>% 
  mutate(Baseline = Dead_Week) %>% 
  select("Baseline")
amb.hw <- heatwave %>%
  filter(Species == "PSME", Treatment_temp == "Ambient_HW") %>% 
  mutate(Heatwave = Dead_Week) %>% 
  select("Heatwave")
PSME <- cbind(amb, amb.hw)
PSME <- PSME %>% 
  mutate(Phase = 1,
         Species = "PSME")

#PIEN
amb <- heatwave %>%
  filter(Species == "PIEN", Treatment_temp == "Ambient") %>% 
  mutate(Baseline = Dead_Week) %>% 
  select("Baseline")
amb.hw <- heatwave %>%
  filter(Species == "PIEN", Treatment_temp == "Ambient_HW") %>% 
  mutate(Heatwave = Dead_Week) %>% 
  select("Heatwave")
PIEN <- cbind(amb, amb.hw)
PIEN <- PIEN %>% 
  mutate(Phase = 1,
         Species = "PIEN")

#Combine all
anova_Dead_Week_Phase1 <- rbind(PIPO, PIED, PIFL, PSME, PIEN)
#anova_Dead_Week_Weight_Phase1 <- rbind(PIPO, PIED, PIFL, PSME, PIEN)
#anova_Dead_Week_Porometer_Phase1 <- rbind(PIPO, PIED, PIFL, PSME, PIEN)

#save csv
#write.csv(anova_Dead_Week, "data/data_analysis/anova_Dead_Week_Phase1.csv", quote=FALSE, row.names = FALSE)
# write.csv(anova_Dead_Week_Weight, "data_analysis/anova_Dead_Week_Weight.csv", quote = FALSE, row.names = FALSE)
# write.csv(anova_Dead_Week_Porometer, "data_analysis/anova_Dead_Week_Porometer.csv", quote = FALSE, row.names = FALSE)


###########
# PHASE 2 #
###########

#PIPO
hot <- heatwave %>%
  filter(Species == "PIPO", Treatment_temp == "Hotter") %>% 
  mutate(Baseline = Dead_Week) %>% 
  select("Baseline")
hot.hw <- heatwave %>%
  filter(Species == "PIPO", Treatment_temp == "Hotter_HW") %>% 
  mutate(Heatwave = Dead_Week) %>% 
  select("Heatwave")
PIPO <- cbind(hot, hot.hw)
PIPO <- PIPO %>% 
  mutate(Phase = 2,
         Species = "PIPO")

#PIED
hot <- heatwave %>%
  filter(Species == "PIED", Treatment_temp == "Hotter") %>% 
  mutate(Baseline = Dead_Week) %>% 
  select("Baseline")
hot.hw <- heatwave %>%
  filter(Species == "PIED", Treatment_temp == "Hotter_HW") %>% 
  mutate(Heatwave = Dead_Week) %>% 
  select("Heatwave")
PIED <- merge(hot, hot.hw, by = 0, all.y = TRUE)
PIED <- PIED %>% 
  select("Baseline","Heatwave")
PIED <- PIED %>% 
  mutate(Phase = 2,
         Species = "PIED")

#PIFL
hot <- heatwave %>%
  filter(Species == "PIFL", Treatment_temp == "Hotter") %>% 
  mutate(Baseline = Dead_Week) %>% 
  select("Baseline")
hot.hw <- heatwave %>%
  filter(Species == "PIFL", Treatment_temp == "Hotter_HW") %>% 
  mutate(Heatwave = Dead_Week) %>% 
  select("Heatwave")
PIFL <- cbind(hot, hot.hw)
PIFL <- PIFL %>% 
  mutate(Phase = 2,
         Species = "PIFL")

#PSME
hot <- heatwave %>%
  filter(Species == "PSME", Treatment_temp == "Hotter") %>% 
  mutate(Baseline = Dead_Week) %>% 
  select("Baseline")
hot.hw <- heatwave %>%
  filter(Species == "PSME", Treatment_temp == "Hotter_HW") %>% 
  mutate(Heatwave = Dead_Week) %>% 
  select("Heatwave")
PSME <- cbind(hot, hot.hw)
PSME <- PSME %>% 
  mutate(Phase = 2,
         Species = "PSME")

#PIEN
hot <- heatwave %>%
  filter(Species == "PIEN", Treatment_temp == "Hotter") %>% 
  mutate(Baseline = Dead_Week) %>% 
  select("Baseline")
hot.hw <- heatwave %>%
  filter(Species == "PIEN", Treatment_temp == "Hotter_HW") %>% 
  mutate(Heatwave = Dead_Week) %>% 
  select("Heatwave")
PIEN <- cbind(hot, hot.hw)
PIEN <- PIEN %>% 
  mutate(Phase = 2,
         Species = "PIEN")

#Combine all
anova_Dead_Week_Phase2 <- rbind(PIPO, PIED, PIFL, PSME, PIEN)
#anova_Dead_Week_Weight_Phase2 <- rbind(PIPO, PIED, PIFL, PSME, PIEN)
#anova_Dead_Week_Porometer_Phase2 <- rbind(PIPO, PIED, PIFL, PSME, PIEN)
#anova_Dead_Week_Weight_Phase2 <- rbind(PIPO, PIED, PIFL, PSME, PIEN)
#anova_Dead_Week_Porometer_Phase2 <- rbind(PIPO, PIED, PIFL, PSME, PIEN)



###########
# COMBINE #
###########
anova_Dead_Week <- rbind(anova_Dead_Week_Phase1, anova_Dead_Week_Phase2)
#anova_Dead_Week_Weight <- rbind(anova_Dead_Week_Phase1, anova_Dead_Week_Phase2)
#anova_Dead_Week_Porometer <- rbind(anova_Dead_Week_Phase1, anova_Dead_Week_Phase2)


#save csv
write.csv(anova_Dead_Week, "data/data_analysis/anova_Dead_Week.csv", quote=FALSE, row.names = FALSE)
# write.csv(anova_Dead_Week_Weight, "data_analysis/anova_Dead_Week_Weight.csv", quote = FALSE, row.names = FALSE)
# write.csv(anova_Dead_Week_Porometer, "data_analysis/anova_Dead_Week_Porometer.csv", quote = FALSE, row.names = FALSE)


################################################################################
# ANOVA
################################################################################
options(digits = 5, show.signif.stars = FALSE)

#read csvs 
anova_Dead_Week <- read_csv("data/data_analysis/anova_Dead_Week.csv")
#anova_Dead_Week_Weight <- read_csv("data_analysis/anova_Dead_Week_Weight.csv")
#anova_Dead_Week_Porometer <- read_csv("data_analysis/anova_Dead_Week_Porometer.csv")

anova_Dead_Week <- anova_Dead_Week %>% 
  filter(Phase == 2)

#weeks from start (anova_Dead_Week)
stripchart(Baseline ~ Species, data = anova_Dead_Week, ylab = 'Dead_Week', pch = 1, col='blue')
aggregate(anova_Dead_Week$Baseline ~ anova_Dead_Week$Species, 
          FUN = function(x) c(n = length(x), mean = mean(x), sd = sd(x))) # group n, mean, and SD


#ANOVA for Baseline species
Dead_Week.aov <- aov(anova_Dead_Week$Baseline ~ anova_Dead_Week$Species, data = anova_Dead_Week)
summary(Dead_Week.aov)    # summary() produces full ANOVA table

#post-hoc multiple comparisons of ALL means ### 
# TukeyHSD() wants an object that holds a fitted model
Dead_Week_HSD <- TukeyHSD(Dead_Week.aov) # Tukey's Honest Significant Differences (HSD)
Dead_Week_HSD_results <- as.data.frame(Dead_Week_HSD$`anova_Dead_Week$Species`)

Dead_Week_HSD <- HSD.test(Dead_Week.aov, trt = "anova_Dead_Week$Species")
Dead_Week_HSD

#ANOVA for heatwave species
Dead_Week_HW.aov <- aov(anova_Dead_Week$Heatwave ~ anova_Dead_Week$Species, data = anova_Dead_Week)
summary(Dead_Week_HW.aov)
#post-hoc multiple comparisons of ALL means ### 
Dead_Week_HW_HSD <- TukeyHSD(Dead_Week_HW.aov) # Tukey's Honest Significant Differences (HSD)
Dead_Week_HW_HSD_results <- as.data.frame(Dead_Week_HW_HSD$`anova_Dead_Week$Species`)

Dead_Week_HW_HSD <- HSD.test(Dead_Week_HW.aov, trt = "anova_Dead_Week$Species")
Dead_Week_HW_HSD

#save csv
write.csv(Dead_Week_HSD_results, "data_analysis/Dead_Week_HSD_results.csv", quote=FALSE, row.names = TRUE)
write.csv(Dead_Week_HW_HSD_results, "data_analysis/Dead_Week_HW_HSD_results.csv", quote = FALSE, row.names = TRUE)


#################################################################################################
#weeks from start of water stress (anova_Dead_Week_Weight)
stripchart(Baseline ~ Species, data = anova_Dead_Week_Weight, ylab = 'Dead_Week', pch = 1, col='blue')
aggregate(anova_Dead_Week_Weight$Baseline ~ anova_Dead_Week_Weight$Species, 
          FUN = function(x) c(n = length(x), mean = mean(x), sd = sd(x))) # group n, mean, and SD

Dead_Week_Weight.aov <- aov(anova_Dead_Week_Weight$Baseline ~ anova_Dead_Week_Weight$Species, data = anova_Dead_Week_Weight)
summary(Dead_Week_Weight.aov)    # summary() produces full ANOVA table

#post-hoc multiple comparisons of ALL means ### 
# TukeyHSD() wants an object that holds a fitted model
Dead_Week_Weight_HSD <- TukeyHSD(Dead_Week_Weight.aov) # Tukey's Honest Significant Differences (HSD)
Dead_Week_Weight_HSD
Dead_Week_Weight_HSD_results <- as.data.frame(Dead_Week_Weight_HSD$`anova_Dead_Week_Weight$Species`)

Dead_Week_Weight_HSD <- HSD.test(Dead_Week_Weight.aov, trt = "anova_Dead_Week_Weight$Species")
Dead_Week_Weight_HSD

#ANOVA for heatwave species
Dead_Week_Weight_HW.aov <- aov(anova_Dead_Week_Weight$Heatwave ~ anova_Dead_Week_Weight$Species, data = anova_Dead_Week_Weight)
summary(Dead_Week_Weight_HW.aov)
#post-hoc multiple comparisons of ALL means ### 
Dead_Week_Weight_HW_HSD <- TukeyHSD(Dead_Week_Weight_HW.aov) # Tukey's Honest Significant Differences (HSD)
Dead_Week_Weight_HW_HSD
Dead_Week_Weight_HW_HSD_results <- as.data.frame(Dead_Week_Weight_HW_HSD$`anova_Dead_Week_Weight$Species`)

Dead_Week_Weight_HW_HSD <- HSD.test(Dead_Week_Weight_HW.aov, trt = "anova_Dead_Week_Weight$Species")
Dead_Week_Weight_HW_HSD

#save csv
write.csv(Dead_Week_Weight_HSD_results, "data_analysis/Dead_Week_Weight_HSD_results.csv", quote=FALSE, row.names = TRUE)
write.csv(Dead_Week_Weight_HW_HSD_results, "data_analysis/Dead_Week_Weight_HW_HSD_results.csv", quote = FALSE, row.names = TRUE)


################################################################################################
#weeks from start of conductance stress (anova_Dead_Week_Porometer)
stripchart(Baseline ~ Species, data = anova_Dead_Week_Porometer, ylab = 'Dead_Week', pch = 1, col='blue')
aggregate(anova_Dead_Week_Porometer$Baseline ~ anova_Dead_Week_Porometer$Species, 
          FUN = function(x) c(n = length(x), mean = mean(x), sd = sd(x))) # group n, mean, and SD

Dead_Week_Porometer.aov <- aov(anova_Dead_Week_Porometer$Baseline ~ anova_Dead_Week_Porometer$Species, data = anova_Dead_Week_Porometer)
summary(Dead_Week_Porometer.aov)    # summary() produces full ANOVA table

#post-hoc multiple comparisons of ALL means ### 
# TukeyHSD() wants an object that holds a fitted model
Dead_Week_Porometer_HSD <- TukeyHSD(Dead_Week_Porometer.aov) # Tukey's Honest Significant Differences (HSD)
Dead_Week_Porometer_HSD
Dead_Week_Porometer_HSD_results <- as.data.frame(Dead_Week_Porometer_HSD$`anova_Dead_Week_Porometer$Species`)

Dead_Week_Porometer_HSD <- HSD.test(Dead_Week_Porometer.aov, trt = "anova_Dead_Week_Porometer$Species")
Dead_Week_Porometer_HSD

#ANOVA for heatwave species
Dead_Week_Porometer_HW.aov <- aov(anova_Dead_Week_Porometer$Heatwave ~ anova_Dead_Week_Porometer$Species, data = anova_Dead_Week_Porometer)
summary(Dead_Week_Porometer_HW.aov)
#post-hoc multiple comparisons of ALL means ### 
Dead_Week_Porometer_HW_HSD <- TukeyHSD(Dead_Week_Porometer_HW.aov) # Tukey's Honest Significant Differences (HSD)
Dead_Week_Porometer_HW_HSD
Dead_Week_Porometer_HW_HSD_results <- as.data.frame(Dead_Week_Porometer_HW_HSD$`anova_Dead_Week_Porometer$Species`)

Dead_Week_Porometer_HW_HSD <- HSD.test(Dead_Week_Porometer_HW.aov, trt = "anova_Dead_Week_Porometer$Species")
Dead_Week_Porometer_HW_HSD

#save csv
write.csv(Dead_Week_Porometer_HSD_results, "data_analysis/Dead_Week_Porometer_HSD_results.csv", quote=FALSE, row.names = TRUE)
write.csv(Dead_Week_Porometer_HW_HSD_results, "data_analysis/Dead_Week_Porometer_HW_HSD_results.csv", quote = FALSE, row.names = TRUE)

