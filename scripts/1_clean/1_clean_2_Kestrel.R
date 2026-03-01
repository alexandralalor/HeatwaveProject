#Data wrangling script - Kestrel
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2026-03-01

#load tidyverse
library(tidyverse)

#read in csv
Phase1_Chamber1_Kestrel1 <- read_csv("data/data_raw/kestrel/Phase1_Chamber1_Kestrel1.csv")
Phase1_Chamber1_Kestrel2 <- read_csv("data/data_raw/kestrel/Phase1_Chamber1_Kestrel2.csv")
Phase1_Chamber2_Kestrel1 <- read_csv("data/data_raw/kestrel/Phase1_Chamber2_Kestrel1.csv")
Phase1_Chamber2_Kestrel2 <- read_csv("data/data_raw/kestrel/Phase1_Chamber2_Kestrel2.csv")
Phase1_Chamber3_Kestrel1 <- read_csv("data/data_raw/kestrel/Phase1_Chamber3_Kestrel1.csv")
Phase1_Chamber3_Kestrel2 <- read_csv("data/data_raw/kestrel/Phase1_Chamber3_Kestrel2.csv")
Phase1_Heatwave_Kestrel1 <- read_csv("data/data_raw/kestrel/Phase1_Heatwave_Kestrel1.csv")
Phase1_Heatwave_Kestrel2 <- read_csv("data/data_raw/kestrel/Phase1_Heatwave_Kestrel2.csv")

Phase2_Chamber1_Kestrel1 <- read_csv("data/data_raw/kestrel/Phase2_Chamber1_Kestrel1.csv")
Phase2_Chamber1_Kestrel2 <- read_csv("data/data_raw/kestrel/Phase2_Chamber1_Kestrel2.csv")
Phase2_Chamber2_Kestrel1 <- read_csv("data/data_raw/kestrel/Phase2_Chamber2_Kestrel1.csv")
Phase2_Chamber2_Kestrel2 <- read_csv("data/data_raw/kestrel/Phase2_Chamber2_Kestrel2.csv")
Phase2_Chamber3_Kestrel1 <- read_csv("data/data_raw/kestrel/Phase2_Chamber3_Kestrel1.csv")
Phase2_Chamber3_Kestrel2 <- read_csv("data/data_raw/kestrel/Phase2_Chamber3_Kestrel2.csv")
Phase2_Heatwave_Kestrel1 <- read_csv("data/data_raw/kestrel/Phase2_Heatwave_Kestrel1.csv")
Phase2_Heatwave_Kestrel2 <- read_csv("data/data_raw/kestrel/Phase2_Heatwave_Kestrel2.csv")



#Combine df
Phase1_Chamber1_Kestrel <- rbind(Phase1_Chamber1_Kestrel1, Phase1_Chamber1_Kestrel2)
Phase1_Chamber2_Kestrel <- rbind(Phase1_Chamber2_Kestrel1, Phase1_Chamber2_Kestrel2)
Phase1_Chamber3_Kestrel <- rbind(Phase1_Chamber3_Kestrel1, Phase1_Chamber3_Kestrel2)
Phase1_Heatwave_Kestrel <- rbind(Phase1_Heatwave_Kestrel1, Phase1_Heatwave_Kestrel2)
Phase1_Kestrel <- rbind(Phase1_Chamber1_Kestrel, Phase1_Chamber2_Kestrel, Phase1_Chamber3_Kestrel, Phase1_Heatwave_Kestrel)

Phase2_Chamber1_Kestrel <- rbind(Phase2_Chamber1_Kestrel1, Phase2_Chamber1_Kestrel2)
Phase2_Chamber2_Kestrel <- rbind(Phase2_Chamber2_Kestrel1, Phase2_Chamber2_Kestrel2)
Phase2_Chamber3_Kestrel <- rbind(Phase2_Chamber3_Kestrel1, Phase2_Chamber3_Kestrel2)
Phase2_Heatwave_Kestrel <- rbind(Phase2_Heatwave_Kestrel1, Phase2_Heatwave_Kestrel2)
Phase2_Kestrel <- rbind(Phase2_Chamber1_Kestrel, Phase2_Chamber2_Kestrel, Phase2_Chamber3_Kestrel, Phase2_Heatwave_Kestrel)


#take a look at data
glimpse(Phase1_Kestrel)


#Convert variables
Phase1_Kestrel$Phase <- as.factor(Phase1_Kestrel$Phase)
Phase1_Kestrel$Chamber <- as.factor(Phase1_Kestrel$Chamber)
Phase1_Kestrel$Kestrel <- as.factor(Phase1_Kestrel$Kestrel)

Phase2_Kestrel$Phase <- as.factor(Phase2_Kestrel$Phase)
Phase2_Kestrel$Chamber <- as.factor(Phase2_Kestrel$Chamber)
Phase2_Kestrel$Kestrel <- as.factor(Phase2_Kestrel$Kestrel)


#DateTime
###Phase 1
Phase1_Kestrel <- Phase1_Kestrel %>%
  separate(Time, sep = " ",
           into = c("Date", "Time")) %>%
  mutate(Date = parse_datetime(Date,
                               format = "%m/%d/%Y"))

Phase1_Kestrel$Time <- gsub(":","",as.factor(Phase1_Kestrel$Time))
Phase1_Kestrel$Time <- as.numeric(Phase1_Kestrel$Time)

Phase1_Kestrel <- Phase1_Kestrel %>% 
  mutate(Time = ifelse(Time < 1000 & Time > 30, paste0("0", Phase1_Kestrel$Time), 
                         ifelse(Time == 30, paste0("00", Phase1_Kestrel$Time), 
                                ifelse(Time == 0, paste0("000", Phase1_Kestrel$Time), Time))))

Phase1_Kestrel$DateTime <- paste(Phase1_Kestrel$Date, " ", Phase1_Kestrel$Time)
Phase1_Kestrel$DateTime <- strptime(Phase1_Kestrel$DateTime, format="%Y-%m-%d %H%M")
Phase1_Kestrel$Date <- as.Date(Phase1_Kestrel$Date)

###Phase 2
Phase2_Kestrel <- Phase2_Kestrel %>%
  separate(Time, sep = " ",
           into = c("Date", "Time")) %>%
  mutate(Date = as.Date(Date)) %>% 
  separate(Time, sep = ":",
           into = c("Hour", "Minute", "Second")) %>% 
  mutate(Time = paste0(Hour, ":", Minute)) %>% 
  relocate(Time, .after = Date) %>% 
  select(!c(Hour, Minute, Second))

Phase2_Kestrel$Time <- gsub(":","",as.factor(Phase2_Kestrel$Time))
Phase2_Kestrel$Time <- as.numeric(Phase2_Kestrel$Time)

Phase2_Kestrel <- Phase2_Kestrel %>% 
  mutate(Time = ifelse(Time < 1000 & Time > 30, paste0("0", Phase2_Kestrel$Time), 
                       ifelse(Time == 30, paste0("00", Phase2_Kestrel$Time), 
                              ifelse(Time == 0, paste0("000", Phase2_Kestrel$Time), Time))))

Phase2_Kestrel$DateTime <- paste(Phase2_Kestrel$Date, " ", Phase2_Kestrel$Time)
Phase2_Kestrel$DateTime <- strptime(Phase2_Kestrel$DateTime, format="%Y-%m-%d %H%M")
Phase2_Kestrel$Date <- as.Date(Phase2_Kestrel$Date)


#combine
Kestrel <- rbind(Phase1_Kestrel, Phase2_Kestrel)


#Save csv
write.csv(Kestrel, "data/data_clean/Kestrel.csv", quote=FALSE, row.names = FALSE)
