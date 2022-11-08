#Data analysis - anova
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-10-21
#Last updated: 2022-10-21

#load packages
library(tidyverse)

#read csvs
Phase1_Data_All <- read_csv("data_analysis/Phase1_Data_All.csv")
Phase1_Data_All_Avg <- read_csv("data_analysis/Phase1_Data_All_Avg.csv")

#dead count
drought <- Phase1_Data_All %>% 
  group_by(Species, Treatment_water, SpeciesID) %>% 
  summarize(Dead_Count = max(Dead_Count, na.rm = T))

#save csv
write.csv(drought, "data_analysis/drought.csv", quote = FALSE, row.names = FALSE)

#read csv
drought <- read_csv("data_analysis/drought.csv")


#PIPO
w <- drought %>%
  filter(Species == "PIPO", Treatment_water == "Watered") %>% 
  mutate(Watered = Dead_Count) %>% 
  select("Watered")
d <- drought %>%
  filter(Species == "PIPO", Treatment_water == "Drought") %>% 
  mutate(Drought = Dead_Count) %>% 
  select("Drought")
PIPO <- merge(w, d, by = 0, all = TRUE) %>% 
  arrange(Watered)
PIPO <- PIPO %>% 
  select("Watered","Drought")

#PIED
w <- drought %>%
  filter(Species == "PIED", Treatment_water == "Watered") %>% 
  mutate(Watered = Dead_Count) %>% 
  select("Watered")
d <- drought %>%
  filter(Species == "PIED", Treatment_water == "Drought") %>% 
  mutate(Drought = Dead_Count) %>% 
  select("Drought")
PIED <- merge(w, d, by = 0, all = TRUE) %>% 
  arrange(Watered)
PIED <- PIED %>% 
  select("Watered","Drought")

#PIFL
w <- drought %>%
  filter(Species == "PIFL", Treatment_water == "Watered") %>% 
  mutate(Watered = Dead_Count) %>% 
  select("Watered")
d <- drought %>%
  filter(Species == "PIFL", Treatment_water == "Drought") %>% 
  mutate(Drought = Dead_Count) %>% 
  select("Drought")
PIFL <- merge(w, d, by = 0, all = TRUE) %>% 
  arrange(Watered)
PIFL <- PIFL %>% 
  select("Watered","Drought")

#PSME
w <- drought %>%
  filter(Species == "PSME", Treatment_water == "Watered") %>% 
  mutate(Watered = Dead_Count) %>% 
  select("Watered")
d <- drought %>%
  filter(Species == "PSME", Treatment_water == "Drought") %>% 
  mutate(Drought = Dead_Count) %>% 
  select("Drought")
PSME <- merge(w, d, by = 0, all = TRUE) %>% 
  arrange(Watered)
PSME <- PSME %>% 
  select("Watered","Drought")

#PIEN
w <- drought %>%
  filter(Species == "PIEN", Treatment_water == "Watered") %>% 
  mutate(Watered = Dead_Count) %>% 
  select("Watered")
d <- drought %>%
  filter(Species == "PIEN", Treatment_water == "Drought") %>% 
  mutate(Drought = Dead_Count) %>% 
  select("Drought")
PIEN <- merge(w, d, by = 0, all = TRUE) %>% 
  arrange(Watered)
PIEN <- PIEN %>% 
  select("Watered","Drought")


# t-tests:
# Two-sample t-test, for differences in mean

#PIPO
PIPO.t <- t.test(x = PIPO$Watered,
                 y = PIPO$Drought,
                 alternative = "less",
                 mu = 0,
                 var.equal = TRUE, 
                 conf.level = 0.95)
PIPO.t
#PIED
PIED.t <- t.test(x = PIED$Watered,
                 y = PIED$Drought,
                 #alternative = "less",
                 mu = 0,
                 var.equal = TRUE, 
                 conf.level = 0.95)
PIED.t
#PIFL
PIFL.t <- t.test(x = PIFL$Watered,
                 y = PIFL$Drought,
                 alternative = "less",
                 mu = 0,
                 var.equal = TRUE, 
                 conf.level = 0.95)
PIFL.t
#PSME
PSME.t <- t.test(x = PSME$Watered,
                 y = PSME$Drought,
                 alternative = "less",
                 mu = 0,
                 var.equal = TRUE, 
                 conf.level = 0.95)
PSME.t
#PIEN
PIEN.t <- t.test(x = PIEN$Watered,
                 y = PIEN$Drought,
                 alternative = "less",
                 mu = 0,
                 var.equal = TRUE, 
                 conf.level = 0.95)
PIEN.t



