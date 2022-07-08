#Data wrangling script - Phase 1 plants
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2022-06-11

#load tidyverse
library(tidyverse)

#read in cleaned up csv
#looking very nice :)
Phase1_PIED <- read_csv(file = "data_raw/plant_data_2/Phase1_PIED.csv")
Phase1_PIPO <- read_csv(file = "data_raw/plant_data_2/Phase1_PIPO.csv")
Phase1_PSME <- read_csv(file = "data_raw/plant_data_2/Phase1_PSME.csv")
Phase1_PIFL <- read_csv(file = "data_raw/plant_data_2/Phase1_PIFL.csv")
Phase1_PIEN <- read_csv(file = "data_raw/plant_data_2/Phase1_PIEN.csv")

#connect plant data
Phase1_Plants <- rbind(Phase1_PIED, Phase1_PIPO, Phase1_PSME, Phase1_PIFL, Phase1_PIEN)

#check out data and make sure it looks ok
glimpse(Phase1_Plants)

#save as csv
write.csv(Phase1_plants, "data_clean/Phase1_Plants.csv", quote = FALSE, row.names = FALSE)
