#Data analysis - anova
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-10-14
#Last updated: 2022-10-14

#load packages
library(tidyverse)

#read csvs
#heatwave <- read.csv("data_analysis/Dead_Week.csv")
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

#read csvs 
anova_Dead_Week <- read_csv("data_analysis/anova_Dead_Week.csv")
anova_Dead_Week_Weight <- read_csv("data_analysis/anova_Dead_Week_Weight.csv")
anova_Dead_Week_Porometer <- read_csv("data_analysis/anova_Dead_Week_Porometer.csv")

#weeks from start (anova_Dead_Week)
stripchart(Ambient ~ Species, data = anova_Dead_Week, ylab = 'Dead_Week', pch = 1, col='blue')
aggregate(anova_Dead_Week$Ambient ~ anova_Dead_Week$Species, 
          FUN = function(x) c(n = length(x), mean = mean(x), sd = sd(x))) # group n, mean, and SD

Dead_Week.aov <- aov(Ambient ~ Species, data = anova_Dead_Week)
summary(Dead_Week.aov)    # summary() produces full ANOVA table


#weeks from start of water stress (anova_Dead_Week_Weight)
stripchart(Ambient ~ Species, data = anova_Dead_Week_Weight, ylab = 'Dead_Week', pch = 1, col='blue')
aggregate(anova_Dead_Week_Weight$Ambient ~ anova_Dead_Week_Weight$Species, 
          FUN = function(x) c(n = length(x), mean = mean(x), sd = sd(x))) # group n, mean, and SD

Dead_Week_Weight.aov <- aov(Ambient ~ Species, data = anova_Dead_Week_Weight)
summary(Dead_Week_Weight.aov)    # summary() produces full ANOVA table


#weeks from start of conductance stress (anova_Dead_Week_Porometer)
stripchart(Ambient ~ Species, data = anova_Dead_Week_Porometer, ylab = 'Dead_Week', pch = 1, col='blue')
aggregate(anova_Dead_Week_Porometer$Ambient ~ anova_Dead_Week_Porometer$Species, 
          FUN = function(x) c(n = length(x), mean = mean(x), sd = sd(x))) # group n, mean, and SD

Dead_Week_Porometer.aov <- aov(Ambient ~ Species, data = anova_Dead_Week_Porometer)
summary(Dead_Week_Porometer.aov)    # summary() produces full ANOVA table

