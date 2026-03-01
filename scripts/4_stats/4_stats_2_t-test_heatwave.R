#Data analysis - t-test
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-09-15
#Last updated: 2026-03-01

#load packages
library(tidyverse)

#read csv
heatwave <- read.csv("data/data_analysis/Dead_Week.csv")


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

################################################################################

# t-tests:
# Two-sample t-test, for differences in mean

#PIPO
PIPO.t <- t.test(x = PIPO$Heatwave,
                 y = PIPO$Baseline,
                 alternative = "less",
                 mu = 0,
                 var.equal = TRUE, 
                 conf.level = 0.95)
PIPO.t
#PIED
PIED.t <- t.test(x = PIED$Heatwave,
                 y = PIED$Baseline,
                 alternative = "less",
                 mu = 0,
                 var.equal = TRUE, 
                 conf.level = 0.95)
PIED.t
#PIFL
PIFL.t <- t.test(x = PIFL$Heatwave,
                 y = PIFL$Baseline,
                 alternative = "less",
                 mu = 0,
                 var.equal = TRUE, 
                 conf.level = 0.95)
PIFL.t
#PSME
PSME.t <- t.test(x = PSME$Heatwave,
                 y = PSME$Baseline,
                 alternative = "less",
                 mu = 0,
                 var.equal = TRUE, 
                 conf.level = 0.95)
PSME.t
#PIEN
PIEN.t <- t.test(x = PIEN$Heatwave,
                 y = PIEN$Baseline,
                 alternative = "less",
                 mu = 0,
                 var.equal = TRUE, 
                 conf.level = 0.95)
PIEN.t


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


################################################################################

# t-tests:
# Two-sample t-test, for differences in mean

#PIPO
PIPO.t <- t.test(x = PIPO$Heatwave,
                 y = PIPO$Baseline,
                 alternative = "less",
                 mu = 0,
                 var.equal = TRUE, 
                 conf.level = 0.95)
PIPO.t
#PIED
PIED.t <- t.test(x = PIED$Heatwave,
                 y = PIED$Baseline,
                 alternative = "less",
                 mu = 0,
                 var.equal = TRUE, 
                 conf.level = 0.95)
PIED.t
#PIFL
PIFL.t <- t.test(x = PIFL$Heatwave,
                 y = PIFL$Baseline,
                 alternative = "less",
                 mu = 0,
                 var.equal = TRUE, 
                 conf.level = 0.95)
PIFL.t
#PSME
PSME.t <- t.test(x = PSME$Heatwave,
                 y = PSME$Baseline,
                 alternative = "less",
                 mu = 0,
                 var.equal = TRUE, 
                 conf.level = 0.95)
PSME.t
#PIEN
PIEN.t <- t.test(x = PIEN$Heatwave,
                 y = PIEN$Baseline,
                 alternative = "less",
                 mu = 0,
                 var.equal = TRUE, 
                 conf.level = 0.95)
PIEN.t

