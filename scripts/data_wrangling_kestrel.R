#Data wrangling script - Phase 1 kestrel
#Allie Lalor
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2022-06-12

#load tidyverse
library(tidyverse)
library(ggplot2)

#read in csv
Phase1_TempSettings <- read_csv("data_raw/kestrel/Phase1_TempSettings.csv")
Phase1_Chamber1_Kestrel1 <- read_csv("data_raw/kestrel/Phase1_Chamber1_Kestrel1.csv")
Phase1_Chamber1_Kestrel2 <- read_csv("data_raw/kestrel/Phase1_Chamber1_Kestrel2.csv")
# Phase1_Chamber2_Kestrel1 <- read_csv("data_raw/kestrel/Phase1_Chamber2_Kestrel1.csv")
# Phase1_Chamber2_Kestrel2 <- read_csv("data_raw/kestrel/Phase1_Chamber2_Kestrel2.csv")
# Phase1_Chamber3_Kestrel1 <- read_csv("data_raw/kestrel/Phase1_Chamber3_Kestrel1.csv")
# Phase1_Chamber3_Kestrel2 <- read_csv("data_raw/kestrel/Phase1_Chamber3_Kestrel2.csv")
# Phase1_Chamber4_Kestrel1 <- read_csv("data_raw/kestrel/Phase1_Chamber4_Kestrel1.csv")
# Phase1_Chamber4_Kestrel2 <- read_csv("data_raw/kestrel/Phase1_Chamber4_Kestrel2.csv")

#Combine df
Phase1_Chamber1_Temps <- rbind(Phase1_Chamber1_Kestrel1, Phase1_Chamber1_Kestrel2)
# Phase1_Chamber2_Temps <- rbind(Phase1_Chamber2_Kestrel1, Phase1_Chamber2_Kestrel2)
# Phase1_Chamber3_Temps <- rbind(Phase1_Chamber3_Kestrel1, Phase1_Chamber3_Kestrel2)
# Phase1_Chamber4_Temps <- rbind(Phase1_Chamber4_Kestrel1, Phase1_Chamber4_Kestrel2)
# Phase1_Temps <- rbind(Phase1_Chamber1, Phase1_Chamber2, Phase1_Chamber3, Phase1_Chamber4)

#Convert variables
Phase1_TempSettings$Phase <- as.factor(Phase1_TempSettings$Phase)
Phase1_TempSettings$Chamber <- as.factor(Phase1_TempSettings$Chamber)
Phase1_TempSettings <- Phase1_TempSettings %>% 
  mutate(Kestrel = "calculated")

Phase1_Chamber1_Temps$Chamber <- as.factor(Phase1_Chamber1_Temps$Chamber)
Phase1_Chamber1_Temps$Kestrel <- as.factor(Phase1_Chamber1_Temps$Kestrel)


#DateTime - Temperature Settings
Phase1_TempSettings <- Phase1_TempSettings %>% 
  mutate(Time = ifelse(Time < 1000 & Time > 30, paste0("0", Phase1_TempSettings$Time), 
                       ifelse(Time == 30, paste0("00", Phase1_TempSettings$Time), 
                              ifelse(Time == 0, paste0("000", Phase1_TempSettings$Time), Time))))


#DateTime - Chamber 1
Phase1_Chamber1_Temps <- Phase1_Chamber1_Temps %>%
  separate(Time, sep = " ",
           into = c("Date", "Time")) %>%
  mutate(Date = parse_datetime(Date,
                               format = "%m/%d/%Y"))

Phase1_Chamber1_Temps$Time <- gsub(":","",as.factor(Phase1_Chamber1_Temps$Time))
Phase1_Chamber1_Temps$Time <- as.numeric(Phase1_Chamber1_Temps$Time)

Phase1_Chamber1_Temps <- Phase1_Chamber1_Temps %>% 
  mutate(Time = ifelse(Time < 1000 & Time > 30, paste0("0", Phase1_Chamber1_Temps$Time), 
                         ifelse(Time == 30, paste0("00", Phase1_Chamber1_Temps$Time), 
                                ifelse(Time == 0, paste0("000", Phase1_Chamber1_Temps$Time), Time))))

Phase1_Chamber1_Temps$DateTime <- paste(Phase1_Chamber1_Temps$Date, " ", Phase1_Chamber1_Temps$Time)
Phase1_Chamber1_Temps$DateTime <- strptime(Phase1_Chamber1_Temps$DateTime, format="%Y-%m-%d %H%M")



#Save csv
write.csv(Phase1_TempSettings, "data_clean/Phase1_TempSettings", quote=FALSE, row.names = FALSE)
write.csv(Phase1_Chamber1_Temps, "data_clean/Phase1_Chamber1_Temps", quote=FALSE, row.names = FALSE)



