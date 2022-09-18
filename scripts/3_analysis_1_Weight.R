#Data analysis - weights
#find second derivative of weight curve for every individual
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-08-27
#Last updated: 2022-09-02

#load tidyverse
library(tidyverse)
library(ggplot2)
#library(sjmisc) #for rotate_df
library(rootSolve) #find where y crosses 0
#library(nlme) #from mixed models lecture
#library(lmerTest) #from mixed models lecture
#library(forcats) #from mixed models lecture

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

write.csv(stress_week, "data_analysis/stress_week.csv", quote = FALSE, row.names = FALSE)
