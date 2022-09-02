#Data analysis
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-08-27
#Last updated: 2022-08-27


#trying to think about models...


Phase1_Data <- read_csv("data_QAQC/Phase1_Data.csv")
Phase1_Data_Drought <- Phaae1_Data %>% 
  filter(Treatment_water == "Drought")

plot(Weight_g ~ Week, data = Phase1_Data_Drought)
plot(Porometer ~ Week, data = Phase1_Data_Drought)
plot(PercentBrown ~ Week, data = Phase1_Data_Drought)
plot(Dead_Count ~ Week, data = Phase1_Data_Drought)
