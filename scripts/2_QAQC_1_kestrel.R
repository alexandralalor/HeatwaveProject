#QAQU - Phase 1 kestrel
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-06-17
#Last updated: 2022-06-17

#load tidyverse
library(tidyverse)

################################################################################
#Phase1_Kestrel_AvgTemp
#Temperatures combined (daily temps), keeping Kestrel number intact for verification
################################################################################
#read in clean csvs
Phase1_TempSettings <- read_csv("data_clean/Phase1_TempSettings.csv")
Phase1_Kestrel <- read_csv("data_clean/Phase1_Kestrel.csv")

#check structure, ensure consistent formats
#Phase as <fctr>
#Chamber as <fctr>
#Kestrel as <fctr>
#Heatwave as <fctr>
glimpse(Phase1_TempSettings)
glimpse(Phase1_Kestrel)

Phase1_TempSettings$Phase <- as.factor(Phase1_TempSettings$Phase)
Phase1_TempSettings$Chamber <- as.factor(Phase1_TempSettings$Chamber)
Phase1_TempSettings$Kestrel <- as.factor(Phase1_TempSettings$Kestrel)
Phase1_TempSettings$Heatwave <- as.factor(Phase1_TempSettings$Heatwave)
Phase1_Kestrel$Phase <- as.factor(Phase1_Kestrel$Phase)
Phase1_Kestrel$Chamber <- as.factor(Phase1_Kestrel$Chamber)
Phase1_Kestrel$Kestrel <- as.factor(Phase1_Kestrel$Kestrel)
Phase1_Kestrel$Heatwave <- as.factor(Phase1_Kestrel$Heatwave)


#average temps
Phase1_Kestrel_Chamber1 <- Phase1_Kestrel %>% 
  filter(Heatwave == "no", Chamber == 1) %>%
  filter(Date >= as.Date("2021-09-01")) %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave, Time) %>% 
  summarize(Temperature_avg = mean(Temperature_C))

Phase1_Kestrel_Chamber2 <- Phase1_Kestrel %>% 
  filter(Heatwave == "no", Chamber == 2) %>%
  filter(Date >= as.Date("2021-09-15")) %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave, Time) %>% 
  summarize(Temperature_avg = mean(Temperature_C))

Phase1_Kestrel_Chamber3 <- Phase1_Kestrel %>% 
  filter(Heatwave == "no", Chamber == 3) %>%
  filter(Date >= as.Date("2021-10-06")) %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave, Time) %>% 
  summarize(Temperature_avg = mean(Temperature_C))


#average temps - heatwave
Phase1_Kestrel_Chamber1_HW <- Phase1_Kestrel %>% 
  filter(Heatwave == "yes", Chamber == 1) %>%
  filter(Date >= as.Date("2021-10-07"), Date <= as.Date("2021-10-14")) %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave, Time) %>% 
  summarize(Temperature_avg = mean(Temperature_C))

Phase1_Kestrel_Chamber2_HW <- Phase1_Kestrel %>% 
  filter(Heatwave == "yes", Chamber == 2) %>%
  filter(Date >= as.Date("2021-10-21"), Date <= as.Date("2021-10-28")) %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave, Time) %>% 
  summarize(Temperature_avg = mean(Temperature_C))

Phase1_Kestrel_Chamber3_HW <- Phase1_Kestrel %>% 
  filter(Heatwave == "yes", Chamber == 3) %>%
  filter(Date >= as.Date("2021-11-11"), Date <= as.Date("2021-11-18")) %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave, Time) %>% 
  summarize(Temperature_avg = mean(Temperature_C))


#calculated temp settings
Phase1_TempSettings_Chamber1 <- Phase1_TempSettings %>%
  filter(Heatwave == "no", Chamber == 1) %>%
  group_by(Phase, Chamber, Kestrel, Heatwave, Time) %>% 
  summarize(Temperature_avg = mean(Temperature_C))

Phase1_TempSettings_Chamber2 <- Phase1_TempSettings %>%
  filter(Heatwave == "no", Chamber == 2) %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave, Time) %>% 
  summarize(Temperature_avg = mean(Temperature_C))

Phase1_TempSettings_Chamber3 <- Phase1_TempSettings %>%
  filter(Heatwave == "no", Chamber == 3) %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave, Time) %>% 
  summarize(Temperature_avg = mean(Temperature_C))


#calculated temp settings - heatwave
Phase1_TempSettings_Chamber1_HW <- Phase1_TempSettings %>%
  filter(Heatwave == "yes", Chamber == 1) %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave, Time) %>% 
  summarize(Temperature_avg = mean(Temperature_C))

Phase1_TempSettings_Chamber2_HW <- Phase1_TempSettings %>%
  filter(Heatwave == "yes", Chamber == 2) %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave, Time) %>% 
  summarize(Temperature_avg = mean(Temperature_C))

Phase1_TempSettings_Chamber3_HW <- Phase1_TempSettings %>%
  filter(Heatwave == "yes", Chamber == 3) %>% 
  group_by(Phase, Chamber, Kestrel, Heatwave, Time) %>% 
  summarize(Temperature_avg = mean(Temperature_C))


#combine data
Phase1_Chamber1_AvgTemp_amb <- rbind(Phase1_Kestrel_Chamber1, Phase1_TempSettings_Chamber1)
Phase1_Chamber1_AvgTemp_HW <- rbind(Phase1_Kestrel_Chamber1_HW, Phase1_TempSettings_Chamber1_HW)
Phase1_Chamber1_AvgTemp <- rbind(Phase1_Chamber1_AvgTemp_amb, Phase1_Chamber1_AvgTemp_HW)

Phase1_Chamber2_AvgTemp_amb <- rbind(Phase1_Kestrel_Chamber2, Phase1_TempSettings_Chamber2)
Phase1_Chamber2_AvgTemp_HW <- rbind(Phase1_Kestrel_Chamber2_HW, Phase1_TempSettings_Chamber2_HW)
Phase1_Chamber2_AvgTemp <- rbind(Phase1_Chamber2_AvgTemp_amb, Phase1_Chamber2_AvgTemp_HW)

Phase1_Chamber3_AvgTemp_amb <- rbind(Phase1_Kestrel_Chamber3, Phase1_TempSettings_Chamber3)
Phase1_Chamber3_AvgTemp_HW <- rbind(Phase1_Kestrel_Chamber3_HW, Phase1_TempSettings_Chamber3_HW)
Phase1_Chamber3_AvgTemp <- rbind(Phase1_Chamber3_AvgTemp_amb, Phase1_Chamber3_AvgTemp_HW)

Phase1_Kestrel_AvgTemp <- rbind(Phase1_Chamber1_AvgTemp, Phase1_Chamber2_AvgTemp, Phase1_Chamber3_AvgTemp)


#save csv
# write.csv(Phase1_Chamber1_AvgTemp, "data_QAQC/Phase1_Chamber1_AvgTemp.csv", quote=FALSE, row.names = FALSE)
# write.csv(Phase1_Chamber2_AvgTemp, "data_QAQC/Phase1_Chamber2_AvgTemp.csv", quote=FALSE, row.names = FALSE)
# write.csv(Phase1_Chamber3_AvgTemp, "data_QAQC/Phase1_Chamber3_AvgTemp.csv", quote=FALSE, row.names = FALSE)
write.csv(Phase1_Kestrel_AvgTemp, "data_QAQC/Phase1_Kestrel_AvgTemp.csv", quote=FALSE, row.names = FALSE)





################################################################################
#Phase1_AvgTemp_Sum
#Take average of both kestrels for summarized data (daily temps)
################################################################################

#read csv
Phase1_Kestrel_AvgTemp <- read_csv("data_QAQC/Phase1_Kestrel_AvgTemp.csv")

#separate actual data vs calculated data
Phase1_Kestrel_AvgTemp_Actual <- Phase1_Kestrel_AvgTemp %>% 
  filter(Kestrel == "1" | Kestrel == "2")
Phase1_Kestrel_AvgTemp_Calculated <- Phase1_Kestrel_AvgTemp %>% 
  filter(Kestrel == "calculated")

#change formats to match
Phase1_Kestrel_AvgTemp_Actual$Phase <- as.factor(Phase1_Kestrel_AvgTemp_Actual$Phase)
Phase1_Kestrel_AvgTemp_Actual$Chamber <- as.factor(Phase1_Kestrel_AvgTemp_Actual$Chamber)
Phase1_Kestrel_AvgTemp_Actual$Kestrel <- as.factor(Phase1_Kestrel_AvgTemp_Actual$Kestrel)
Phase1_Kestrel_AvgTemp_Calculated$Phase <- as.factor(Phase1_Kestrel_AvgTemp_Calculated$Phase)
Phase1_Kestrel_AvgTemp_Calculated$Chamber <- as.factor(Phase1_Kestrel_AvgTemp_Calculated$Chamber)

#take a look at data
glimpse(Phase1_Kestrel_AvgTemp_Actual)
glimpse(Phase1_Kestrel_AvgTemp_Calculated)


#average of kestrels
Phase1_Kestrel_AvgTemp_Actual <- Phase1_Kestrel_AvgTemp_Actual %>% 
  group_by(Phase, Chamber, Heatwave, Time) %>% 
  summarize(Temperature_avg = round(mean(Temperature_avg), digits = 1)) %>% 
  mutate(Kestrel = "actual")

#combine average and calculated data
Phase1_Kestrel_AvgTemp_Sum <- rbind(Phase1_Kestrel_AvgTemp_Actual, Phase1_Kestrel_AvgTemp_Calculated)

#write csv
write.csv(Phase1_Kestrel_AvgTemp_Sum, "data_QAQC/Phase1_Kestrel_AvgTemp_Sum.csv", quote=FALSE, row.names = FALSE)


################################################################################
#Phase1_Kestrel_AvgTemp_Sum_Total
#Take average for overall temp summary (not daily)
################################################################################

#read csv
Phase1_Kestrel_AvgTemp_Sum <- read_csv("data_QAQC/Phase1_Kestrel_AvgTemp_Sum.csv")

Phase1_Kestrel_AvgTemp_Sum_Total <- Phase1_Kestrel_AvgTemp_Sum %>% 
  group_by(Phase, Chamber, Heatwave, Kestrel) %>% 
  summarize(Temperature_avg = round(mean(Temperature_avg), digits = 1))

#write csv
write.csv(Phase1_Kestrel_AvgTemp_Sum_Total, "data_QAQC/Phase1_Kestrel_AvgTemp_Sum_Total.csv", quote=FALSE, row.names = FALSE)

