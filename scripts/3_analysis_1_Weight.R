#Data analysis - weights stress points
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-08-27
#Last updated: 2022-09-18

#load packages
library(tidyverse)
library(ggplot2)
library(rootSolve) #find where y crosses 0

################################################################################
#find second derivative of weight curve for every individual
################################################################################

#read CSVs
Phase1_Data_Weight <- read_csv("data_QAQC/Phase1_Data_Weight.csv")
Phase1_InitialData <- read_csv("data_clean/Phase1_InitialData.csv")

#smooth.spline doesn't allow NA values, so filter them out
Phase1_Data_Weight <- Phase1_Data_Weight %>% 
  filter(!is.na(Weight_g), Treatment_water == "Drought")
Phase1_InitialData <- Phase1_InitialData %>% 
  filter(Treatment_water == "Drought")

#make empty files names df
stress_week <- data.frame(matrix(ncol = 0, nrow = 0))
#define paths
SpeciesID <- Phase1_InitialData$SpeciesID

#for loop to compile stress points
for(i in 1:length(SpeciesID)) {
  ID <- SpeciesID[i]
  Phase1_Data_Weight_filter <- Phase1_Data_Weight %>% 
    filter(SpeciesID == ID)
  smooth <- smooth.spline(x = Phase1_Data_Weight_filter$Week,
                          y = Phase1_Data_Weight_filter$Weight_g)
  predict_d2 <- predict(smooth, deriv=2)
  stress_week_1 <- as.matrix(uniroot.all(approxfun(predict_d2$x, predict_d2$y),
                                         interval = range(predict_d2$x)))
  colnames(stress_week_1) <- ID
  stress_week <- merge(stress_week, stress_week_1, by = 0, all = T)
  stress_week <- stress_week %>% 
    select(c(-"Row.names"))
}

#reformat stress week
stress_week <- gather(stress_week, "SpeciesID", "Stress_Week")
stress_week <- stress_week %>% 
  filter(!is.na(Stress_Week))

# add meta data
file_add <- Phase1_InitialData %>%
  filter(Treatment_water == "Drought") %>% 
  select(c("Species","SpeciesID"))

stress_week <- merge(stress_week, file_add, by = "SpeciesID")

write.csv(stress_week, "data_QAQC/stress_week_weight.csv", quote = FALSE, row.names = FALSE)


################################################################################
#determine which derivative value is correct for every individual
################################################################################

#read  csv
stress_week <- read.csv("data_QAQC/stress_week_weight.csv")
Phase1_Data_Weight <- read_csv("data_QAQC/Phase1_Data_Weight.csv")

#exclude false points with low values (early weeks)
#outlier individuals specified in filters

#PIED
stress_week_PIED <- stress_week %>% 
  filter(Species == "PIED", ifelse(SpeciesID == "PIED41", Stress_Week > 7, 
                                   Stress_Week > 2.4))
#PIPO
stress_week_PIPO <- stress_week %>% 
  filter(Species == "PIPO", Stress_Week > 3.23)
#PIFL
stress_week_PIFL <- stress_week %>% 
  filter(Species == "PIFL", ifelse(SpeciesID == "PIFL08" | SpeciesID == "PIFL11" |
                                     SpeciesID == "PIFL14" | SpeciesID == "PIFL15" |
                                     SpeciesID == "PIFL17" | SpeciesID == "PIFL18" |
                                     SpeciesID == "PIFL21" | SpeciesID == "PIFL22" |
                                     SpeciesID == "PIFL23", Stress_Week > 7,
                                   ifelse(SpeciesID == "PIFL25", Stress_Week > 8, 
                                          ifelse(SpeciesID == "PIFL39", Stress_Week > 15, 
                                                 Stress_Week > 9.87))))
#PSME
stress_week_PSME <- stress_week %>% 
  filter(Species == "PSME", ifelse(SpeciesID == "PSME07" | SpeciesID == "PSME13" |
                                     SpeciesID == "PSME15" | SpeciesID == "PSME18" |
                                     SpeciesID == "PSME19" | SpeciesID == "PSME22" |
                                     SpeciesID == "PSME32" | SpeciesID == "PSME45" | 
                                     SpeciesID == "PSME49", Stress_Week > 5, 
                                   Stress_Week > 7))
#PIEN
stress_week_PIEN <- stress_week %>% 
  filter(Species == "PIEN", ifelse(SpeciesID == "PIEN39", Stress_Week > 8,
                                   Stress_Week > 7))


#find min as when plant first was drought stressed

#PIED
stress_week_PIED <- stress_week_PIED %>% 
  group_by(SpeciesID) %>% 
  mutate(min = min(Stress_Week)) %>% 
  filter(Stress_Week == min) %>% 
  select(-min)
#PIPO
stress_week_PIPO <- stress_week_PIPO %>% 
  group_by(SpeciesID) %>% 
  mutate(min = min(Stress_Week)) %>% 
  filter(Stress_Week == min) %>% 
  select(-min)
#PIFL
stress_week_PIFL <- stress_week_PIFL %>% 
  group_by(SpeciesID) %>% 
  mutate(min = min(Stress_Week)) %>% 
  filter(Stress_Week == min) %>% 
  select(-min)
#PSME
stress_week_PSME <- stress_week_PSME %>% 
  group_by(SpeciesID) %>% 
  mutate(min = min(Stress_Week)) %>% 
  filter(Stress_Week == min) %>% 
  select(-min)
#PIEN
stress_week_PIEN <- stress_week_PIEN %>% 
  group_by(SpeciesID) %>% 
  mutate(min = min(Stress_Week)) %>% 
  filter(Stress_Week == min) %>% 
  select(-min)

#condense stress week data
stress_week_new <- rbind(stress_week_PIED,
                         stress_week_PIPO,
                         stress_week_PIFL,
                         stress_week_PSME,
                         stress_week_PIEN)
stress_week_new <- stress_week_new %>% 
  select(c("SpeciesID","Stress_Week"))

#add to Phase1_Data_Weights
Phase1_Data_Weight <- merge(Phase1_Data_Weight, stress_week_new, by = "SpeciesID", all = T)

#reorder and rearrange columns
Phase1_Data_Weight <- Phase1_Data_Weight[, c(2,3,4,5,6,1,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28)]
Phase1_Data_Weight <- Phase1_Data_Weight %>% 
  group_by(Species) %>% 
  arrange(SpeciesID, Week)


#save csv
write.csv(Phase1_Data_Weight, "data_analysis/Phase1_Data_Weight.csv", quote = FALSE, row.names = FALSE)


################################################################################
# #validataion
# 
# stress_week_filter <- stress_week_PIED %>% 
#   filter(SpeciesID == "PIED06")
#
# #Weight over time (averaged)
# Phase1_Data_Weight %>% 
#   filter(Treatment_water == "Drought") %>% 
#   filter(SpeciesID == "PIED06") %>% 
#   ggplot(aes(x = Week,
#              y = Weight_g,
#              color = Treatment_water)) +
#   geom_point() +
#   geom_line() +
#   annotate("segment",
#            x = stress_week_filter$Stress_Week, xend = stress_week_filter$Stress_Week,
#            y = 200, yend = 800,
#            color = "black",
#            linetype = "dashed",
#            size = 0.6) +
#   annotate("segment",
#            x = 7, xend = 7,
#            y = 200, yend = 800,
#            color = "red",
#            linetype = "dashed",
#            size = 0.6) +
#   ylim(200, 950) +
#   xlim(0,36) +
#   #facet_wrap(~SpeciesID) +
#   xlab("Week") +
#   ylab("Water Weight (g)") +
#   labs(title = "Weight of Droughted Trees") +
#   theme_minimal()
