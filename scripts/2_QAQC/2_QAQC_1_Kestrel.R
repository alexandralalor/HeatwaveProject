#QAQU - Phase 1 kestrel
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-06-17
#Last updated: 2026-03-01

#load tidyverse
library(tidyverse)

################################################################################
#Kestrel_AvgTemp
#Temperatures combined (daily temps), keeping Kestrel number intact for verification
################################################################################
#read in clean csvs
TempSettings <- read_csv("data/data_clean/TempSettings.csv")
Kestrel <- read_csv("data/data_clean/Kestrel.csv")

#check structure, ensure consistent formats
#Phase as <fctr>
#Chamber as <fctr>
#Kestrel as <fctr>
#Heatwave as <fctr>
glimpse(TempSettings)
glimpse(Kestrel)

TempSettings$Phase <- as.factor(TempSettings$Phase)
TempSettings$Chamber <- as.factor(TempSettings$Chamber)
TempSettings$Kestrel <- as.factor(TempSettings$Kestrel)
TempSettings$Heatwave <- as.factor(TempSettings$Heatwave)
Kestrel$Phase <- as.factor(Kestrel$Phase)
Kestrel$Chamber <- as.factor(Kestrel$Chamber)
Kestrel$Kestrel <- as.factor(Kestrel$Kestrel)
Kestrel$Heatwave <- as.factor(Kestrel$Heatwave)


#average temps
Kestrel_Chamber1 <- Kestrel %>% 
  filter(Heatwave == "no", Chamber == 1) %>%
  #filter(Date >= as.Date("2021-09-01")) %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave, Time) %>% 
  summarize(Temperature_avg = mean(Temperature_C))

Kestrel_Chamber2 <- Kestrel %>% 
  filter(Heatwave == "no", Chamber == 2) %>%
  #filter(Date >= as.Date("2021-09-15")) %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave, Time) %>% 
  summarize(Temperature_avg = mean(Temperature_C))

Kestrel_Chamber3 <- Kestrel %>% 
  filter(Heatwave == "no", Chamber == 3) %>%
  #filter(Date >= as.Date("2021-10-06")) %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave, Time) %>% 
  summarize(Temperature_avg = mean(Temperature_C))


#average temps - heatwave
Kestrel_Chamber1_HW <- Kestrel %>% 
  filter(Heatwave == "yes", Chamber == 1) %>%
  #filter(Date >= as.Date("2021-10-07"), Date <= as.Date("2021-10-14")) %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave, Time) %>% 
  summarize(Temperature_avg = mean(Temperature_C))

Kestrel_Chamber2_HW <- Kestrel %>% 
  filter(Heatwave == "yes", Chamber == 2) %>%
  #filter(Date >= as.Date("2021-10-21"), Date <= as.Date("2021-10-28")) %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave, Time) %>% 
  summarize(Temperature_avg = mean(Temperature_C))

Kestrel_Chamber3_HW <- Kestrel %>% 
  filter(Heatwave == "yes", Chamber == 3) %>%
  #filter(Date >= as.Date("2021-11-11"), Date <= as.Date("2021-11-18")) %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave, Time) %>% 
  summarize(Temperature_avg = mean(Temperature_C))


#calculated temp settings
TempSettings_Chamber1 <- TempSettings %>%
  filter(Heatwave == "no", Chamber == 1) %>%
  group_by(Phase, Chamber, Kestrel, Heatwave, Time) %>% 
  summarize(Temperature_avg = mean(Temperature_C))

TempSettings_Chamber2 <- TempSettings %>%
  filter(Heatwave == "no", Chamber == 2) %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave, Time) %>% 
  summarize(Temperature_avg = mean(Temperature_C))

TempSettings_Chamber3 <- TempSettings %>%
  filter(Heatwave == "no", Chamber == 3) %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave, Time) %>% 
  summarize(Temperature_avg = mean(Temperature_C))


#calculated temp settings - heatwave
TempSettings_Chamber1_HW <- TempSettings %>%
  filter(Heatwave == "yes", Chamber == 1) %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave, Time) %>% 
  summarize(Temperature_avg = mean(Temperature_C))

TempSettings_Chamber2_HW <- TempSettings %>%
  filter(Heatwave == "yes", Chamber == 2) %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave, Time) %>% 
  summarize(Temperature_avg = mean(Temperature_C))

TempSettings_Chamber3_HW <- TempSettings %>%
  filter(Heatwave == "yes", Chamber == 3) %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave, Time) %>% 
  summarize(Temperature_avg = mean(Temperature_C))


#combine data
Chamber1_AvgTemp_amb <- rbind(Kestrel_Chamber1, TempSettings_Chamber1)
Chamber1_AvgTemp_HW <- rbind(Kestrel_Chamber1_HW, TempSettings_Chamber1_HW)
Chamber1_AvgTemp <- rbind(Chamber1_AvgTemp_amb, Chamber1_AvgTemp_HW)

Chamber2_AvgTemp_amb <- rbind(Kestrel_Chamber2, TempSettings_Chamber2)
Chamber2_AvgTemp_HW <- rbind(Kestrel_Chamber2_HW, TempSettings_Chamber2_HW)
Chamber2_AvgTemp <- rbind(Chamber2_AvgTemp_amb, Chamber2_AvgTemp_HW)

Chamber3_AvgTemp_amb <- rbind(Kestrel_Chamber3, TempSettings_Chamber3)
Chamber3_AvgTemp_HW <- rbind(Kestrel_Chamber3_HW, TempSettings_Chamber3_HW)
Chamber3_AvgTemp <- rbind(Chamber3_AvgTemp_amb, Chamber3_AvgTemp_HW)

Kestrel_AvgTemp <- rbind(Chamber1_AvgTemp, Chamber2_AvgTemp, Chamber3_AvgTemp)


#save csv
write.csv(Kestrel_AvgTemp, "data/data_QAQC/Kestrel_AvgTemp.csv", quote=FALSE, row.names = FALSE)





################################################################################
#AvgTemp_Sum
#Take average of both kestrels for summarized data (daily temps)
################################################################################

#read csv
Kestrel_AvgTemp <- read_csv("data/data_QAQC/Kestrel_AvgTemp.csv")

#separate actual data vs calculated data
Kestrel_AvgTemp_Actual <- Kestrel_AvgTemp %>% 
  filter(Kestrel == "1" | Kestrel == "2")
Kestrel_AvgTemp_Calculated <- Kestrel_AvgTemp %>% 
  filter(Kestrel == "calculated")

#change formats to match
Kestrel_AvgTemp_Actual$Phase <- as.factor(Kestrel_AvgTemp_Actual$Phase)
Kestrel_AvgTemp_Actual$Chamber <- as.factor(Kestrel_AvgTemp_Actual$Chamber)
Kestrel_AvgTemp_Actual$Kestrel <- as.factor(Kestrel_AvgTemp_Actual$Kestrel)
Kestrel_AvgTemp_Calculated$Phase <- as.factor(Kestrel_AvgTemp_Calculated$Phase)
Kestrel_AvgTemp_Calculated$Chamber <- as.factor(Kestrel_AvgTemp_Calculated$Chamber)

#take a look at data
glimpse(Kestrel_AvgTemp_Actual)
glimpse(Kestrel_AvgTemp_Calculated)


#average of kestrels
Kestrel_AvgTemp_Actual <- Kestrel_AvgTemp_Actual %>% 
  group_by(Phase, Chamber, Heatwave, Time) %>% 
  summarize(Temperature_avg = round(mean(Temperature_avg), digits = 1)) %>% 
  mutate(Kestrel = "actual")

#combine average and calculated data
Kestrel_AvgTemp_Sum <- rbind(Kestrel_AvgTemp_Actual, Kestrel_AvgTemp_Calculated)

#write csv
write.csv(Kestrel_AvgTemp_Sum, "data/data_QAQC/Kestrel_AvgTemp_Sum.csv", quote=FALSE, row.names = FALSE)


################################################################################
#Kestrel_AvgTemp_Sum_Total
#Take average for overall temp summary (not daily)
################################################################################

#read csv
Kestrel_AvgTemp_Sum <- read_csv("data/data_QAQC/Kestrel_AvgTemp_Sum.csv")

Kestrel_AvgTemp_Sum_Total <- Kestrel_AvgTemp_Sum %>% 
  group_by(Phase, Chamber, Heatwave, Kestrel) %>% 
  summarize(Temperature_avg = round(mean(Temperature_avg), digits = 1))

#write csv
write.csv(Kestrel_AvgTemp_Sum_Total, "data/data_QAQC/Kestrel_AvgTemp_Sum_Total.csv", quote=FALSE, row.names = FALSE)

