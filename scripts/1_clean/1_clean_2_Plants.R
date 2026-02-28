#Data wrangling script - Plants
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2026-02-28

#load tidyverse
library(tidyverse)

#read in cleaned up csv
#looking very nice :)
PIED <- read_csv(file = "data_raw/plant_data_2/PIED.csv")
PIPO <- read_csv(file = "data_raw/plant_data_2/PIPO.csv")
PSME <- read_csv(file = "data_raw/plant_data_2/PSME.csv")
PIFL <- read_csv(file = "data_raw/plant_data_2/PIFL.csv")
PIEN <- read_csv(file = "data_raw/plant_data_2/PIEN.csv")

#connect plant data
Plants <- rbind(PIED, PIPO, PSME, PIFL, PIEN)

#check out data and make sure it looks ok
glimpse(Plants)

#save as csv
write.csv(Plants, "data_clean/Plants.csv", quote = FALSE, row.names = FALSE)
