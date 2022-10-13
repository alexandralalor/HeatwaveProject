#Data analysis - t-test
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-09-15
#Last updated: 2022-10-10

#load packages
library(tidyverse)

#read csv
heatwave <- read.csv("data_analysis/Dead_Week.csv")

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

################################################################################

# t-tests:
# Two-sample t-test, for differences in mean

#PIPO
PIPO.t <- t.test(x = PIPO$Heatwave,
                 y = PIPO$Ambient,
                 alternative = "less",
                 mu = 0,
                 var.equal = TRUE, 
                 conf.level = 0.95)
PIPO.t
#PIED
PIED.t <- t.test(x = PIED$Heatwave,
                 y = PIED$Ambient,
                 alternative = "less",
                 mu = 0,
                 var.equal = TRUE, 
                 conf.level = 0.95)
PIED.t
#PIFL
PIFL.t <- t.test(x = PIFL$Heatwave,
                 y = PIFL$Ambient,
                 alternative = "less",
                 mu = 0,
                 var.equal = TRUE, 
                 conf.level = 0.95)
PIFL.t
#PSME
PSME.t <- t.test(x = PSME$Heatwave,
                 y = PSME$Ambient,
                 alternative = "less",
                 mu = 0,
                 var.equal = TRUE, 
                 conf.level = 0.95)
PSME.t
#PIEN
PIEN.t <- t.test(x = PIEN$Heatwave,
                 y = PIEN$Ambient,
                 alternative = "less",
                 mu = 0,
                 var.equal = TRUE, 
                 conf.level = 0.95)
PIEN.t

