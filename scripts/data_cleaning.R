#data cleaning and checking


#working directory
setwd("~/Desktop/UofA/HW project/analysis/HeatwaveProject")

#load tidyverse
library(tidyverse)

#read in cleaned up csv
#looking very nice :)
Phase1_PIED_old <- read.csv(file = "data_step1/Phase1_PIED.csv")
Phase1_PIPO_old <- read.csv(file = "data_step1/Phase1_PIPO.csv")
Phase1_PSME_old <- read.csv(file = "data_step1/Phase1_PSME.csv")
Phase1_InitialData <- read.csv(file = "data_step1/Phase1_InitialData.csv")

#not sure why there's an extra column with the row #s... delete
#delete first column
#Phase1_PIED <- select(Phase1_PIED_old, -1)
#Phase1_PSME <- select(Phase1_PSME_old, -1)
#Phase1_PIPO <- select(Phase1_PIPO_old, -1)

#connect data
?rbind
Phase1_all <- rbind(Phase1_PIED, Phase1_PIPO, Phase1_PSME)

#temp
Phase1_all_temp <- rbind(Phase1_PIED_PIEN, Phase1_PIPO, Phase1_PSME)

#now add metadata
Phase1_InitialData_Treatments <- subset(Phase1_InitialData, ,c(2, 4:7))

Phase1_meta <- merge(Phase1_all,
                     Phase1_InitialData_Treatments,
                     by=c("SpeciesID"))
#temp
Phase1_meta_temp <- merge(Phase1_all_temp,
                     Phase1_InitialData_Treatments,
                     by=c("SpeciesID"))

#Error checking
#PercentBrown as 10, 25, 50, 75, 90
#What else?????????


  


