#Data wrangling script - Phase 1 kestrel
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2022-06-12

#load tidyverse
library(tidyverse)

#read in csv
Phase1_Chamber1_Kestrel1 <- read_csv("data_raw/kestrel/Phase1_Chamber1_Kestrel1.csv")
Phase1_Chamber1_Kestrel2 <- read_csv("data_raw/kestrel/Phase1_Chamber1_Kestrel2.csv")
Phase1_Chamber2_Kestrel1 <- read_csv("data_raw/kestrel/Phase1_Chamber2_Kestrel1.csv")
Phase1_Chamber2_Kestrel2 <- read_csv("data_raw/kestrel/Phase1_Chamber2_Kestrel2.csv")
Phase1_Chamber3_Kestrel1 <- read_csv("data_raw/kestrel/Phase1_Chamber3_Kestrel1.csv")
Phase1_Chamber3_Kestrel2 <- read_csv("data_raw/kestrel/Phase1_Chamber3_Kestrel2.csv")
Phase1_Heatwave_Kestrel1 <- read_csv("data_raw/kestrel/Phase1_Heatwave_Kestrel1.csv")
Phase1_Heatwave_Kestrel2 <- read_csv("data_raw/kestrel/Phase1_Heatwave_Kestrel2.csv")

Phase1_Kestrel_Meta <- read_csv("data_raw/kestrel/Phase1_Kestrel_Meta.csv")

#Combine df
Phase1_Chamber1_Kestrel <- rbind(Phase1_Chamber1_Kestrel1, Phase1_Chamber1_Kestrel2)
Phase1_Chamber2_Kestrel <- rbind(Phase1_Chamber2_Kestrel1, Phase1_Chamber2_Kestrel2)
Phase1_Chamber3_Kestrel <- rbind(Phase1_Chamber3_Kestrel1, Phase1_Chamber3_Kestrel2)
Phase1_Heatwave_Kestrel <- rbind(Phase1_Heatwave_Kestrel1, Phase1_Heatwave_Kestrel2)
Phase1_Kestrel <- rbind(Phase1_Chamber1_Kestrel, Phase1_Chamber2_Kestrel, Phase1_Chamber3_Kestrel, Phase1_Heatwave_Kestrel)

#take a look at data
glimpse(Phase1_Kestrel)
glimpse(Phase1_Kestrel_Meta)

#Convert variables
Phase1_Kestrel$Phase <- as.factor(Phase1_Kestrel$Phase)
Phase1_Kestrel$Chamber <- as.factor(Phase1_Kestrel$Chamber)
Phase1_Kestrel$Kestrel <- as.factor(Phase1_Kestrel$Kestrel)

Phase1_Kestrel_Meta$Phase <- as.factor(Phase1_Kestrel_Meta$Phase)
Phase1_Kestrel_Meta$Chamber <- as.factor(Phase1_Kestrel_Meta$Chamber)
Phase1_Kestrel_Meta$Kestrel <- as.factor(Phase1_Kestrel_Meta$Kestrel)


#DateTime - Chamber 1
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


#take another look at data
glimpse(Phase1_Kestrel)
glimpse(Phase1_Kestrel_Meta)

#merge Kestrel + Kestrel_Meta data
Phase1_Kestrel <- merge(Phase1_Kestrel, Phase1_Kestrel_Meta, by = c("Phase", "Chamber", "Kestrel", "Heatwave"))


#take another look at data
glimpse(Phase1_Kestrel)

#Save csv
write.csv(Phase1_Kestrel, "data_clean/Phase1_Kestrel.csv", quote=FALSE, row.names = FALSE)
