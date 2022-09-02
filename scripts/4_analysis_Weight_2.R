#Data analysis - weights
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-08-27
#Last updated: 2022-09-02

#load tidyverse
library(tidyverse)
library(ggplot2)
library(sjmisc) #for rotate_df
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
  filter(!is.na(Week))

# add meta data
file_add <- Phase1_InitialData %>%
  filter(Treatment_water == "Drought") %>% 
  select(c("Species","SpeciesID"))

stress_week <- merge(stress_week, file_add, by = "SpeciesID")

write.csv(stress_week, "data_analysis/stress_week.csv", quote = FALSE, row.names = FALSE)

##########################################################################

#read  csv
stress_week <- read.csv("data_analysis/stress_week.csv")
Phase1_Data_Weight <- read_csv("data_QAQC/Phase1_Data_Weight.csv")

#exclude false points with low values

#PIED
#PIED 41 weird...
stress_week_PIED <- stress_week %>% 
  filter(Species == "PIED", ifelse(SpeciesID == "PIED41", Week > 7, Week > 2.4))

#PIPO
stress_week_PIPO <- stress_week %>% 
  filter(Species == "PIPO", Week > 3.23)

#PIFL
stress_week_PIFL <- stress_week %>% 
  filter(Species == "PIFL", ifelse(SpeciesID == "PIFL08" | SpeciesID == "PIFL11" |
                                     SpeciesID == "PIFL14" | SpeciesID == "PIFL15" |
                                     SpeciesID == "PIFL17" | SpeciesID == "PIFL18" |
                                     SpeciesID == "PIFL21" | SpeciesID == "PIFL22" |
                                     SpeciesID == "PIFL23", Week > 7,
                                   ifelse(SpeciesID == "PIFL25", Week > 8, 
                                          ifelse(SpeciesID == "PIFL39", Week > 15, Week > 9.87))))

#PSME
stress_week_PSME <- stress_week %>% 
  filter(Species == "PSME", ifelse(SpeciesID == "PSME07" | SpeciesID == "PSME13" |
                                     SpeciesID == "PSME15" | SpeciesID == "PSME18" |
                                     SpeciesID == "PSME19" | SpeciesID == "PSME22" |
                                     SpeciesID == "PSME32" | SpeciesID == "PSME45" | 
                                     SpeciesID == "PSME49", Week > 5, 
                                   Week > 7))

#PIEN
stress_week_PIEN <- stress_week %>% 
  filter(Species == "PIEN", ifelse(SpeciesID == "PIEN39", Week > 8,
                                   Week > 7))


#find min as when plant first was drought stressed
#PIED
stress_week_PIED <- stress_week_PIED %>% 
  group_by(SpeciesID) %>% 
  mutate(min = min(Week)) %>% 
  filter(Week == min) %>% 
  select(-min)
#PIPO
stress_week_PIPO <- stress_week_PIPO %>% 
  group_by(SpeciesID) %>% 
  mutate(min = min(Week)) %>% 
  filter(Week == min) %>% 
  select(-min)
#PIFL
stress_week_PIFL <- stress_week_PIFL %>% 
  group_by(SpeciesID) %>% 
  mutate(min = min(Week)) %>% 
  filter(Week == min) %>% 
  select(-min)
#PSME
stress_week_PSME <- stress_week_PSME %>% 
  group_by(SpeciesID) %>% 
  mutate(min = min(Week)) %>% 
  filter(Week == min) %>% 
  select(-min)
#PIEN
stress_week_PIEN <- stress_week_PIEN %>% 
  group_by(SpeciesID) %>% 
  mutate(min = min(Week)) %>% 
  filter(Week == min) %>% 
  select(-min)



stress_week_filter <- stress_week_PIED %>% 
  filter(Species == "PIED")


#validataion
#Weight over time (averaged)
Phase1_Data_Weight %>% 
  filter(Treatment_water == "Drought") %>% 
  filter(Species == "PIED") %>% 
  ggplot(aes(x = Week,
             y = Weight_g,
             color = Treatment_water)) +
  geom_point() +
  #geom_line() +
  annotate("segment",
           x = stress_week_filter$Week, xend = stress_week_filter$Week,
           y = 200, yend = 800,
           color = "black",
           linetype = "dashed",
           size = 0.6) +
  annotate("segment",
           x = 7, xend = 7,
           y = 200, yend = 800,
           color = "red",
           linetype = "dashed",
           size = 0.6) +
  ylim(200, 950) +
  xlim(0,36) +
  #facet_wrap(~SpeciesID) +
  xlab("Week") +
  ylab("Water Weight (g)") +
  labs(title = "Weight of Droughted Trees") +
  theme_minimal()

#condense stress week data
stress_week_new <- rbind(stress_week_PIED,
                         stress_week_PIPO,
                         stress_week_PIFL,
                         stress_week_PSME,
                         stress_week_PIEN)
stress_week_new <- stress_week_new %>% 
  mutate(Stress_Week = Week)
stress_week_new <- stress_week_new %>% 
  select(c("SpeciesID","Stress_Week"))

#add to Phase1_Data_Weights
Phase1_Data_Weight <- merge(Phase1_Data_Weight, stress_week_new, by = "SpeciesID", all = T)

#reorder and rearrange columns
Phase1_Data_Weight <- Phase1_Data_Weight[, c(2,3,4,5,6,1,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26)]
Phase1_Data_Weight <- Phase1_Data_Weight %>% 
  group_by(Species) %>% 
  arrange(SpeciesID, Week)

#save csv
write.csv(Phase1_Data_Weight, "data_analysis/Phase1_Data_Weight.csv", quote = FALSE, row.names = FALSE)


# #filter data
# Phase1_Data_Weight <- Phase1_Data_Weight %>% 
#   filter(!is.na(Weight_Est), Treatment_water == "Drought")
# 
# #separate species
# Phase1_Data_Weight_PIPO <- Phase1_Data_Weight %>% 
#   filter(Species == "PIPO")
# Phase1_Data_Weight_PIED <- Phase1_Data_Weight %>% 
#   filter(Species == "PIED")
# Phase1_Data_Weight_PIFL <- Phase1_Data_Weight %>% 
#   filter(Species == "PIFL")
# Phase1_Data_Weight_PSME <- Phase1_Data_Weight %>% 
#   filter(Species == "PSME")
# Phase1_Data_Weight_PIEN <- Phase1_Data_Weight %>% 
#   filter(Species == "PIEN")


# # plain and simple, find 2nd derivative
# #PIPO
# smooth_PIPO <- smooth.spline(x=Phase1_Data_Weight_PIPO$Week, 
#                               y=Phase1_Data_Weight_PIPO$Weight_Est)
# predict_PIPO <- predict(smooth_PIPO)
# predict_PIPO.prime.2 <- predict(smooth_PIPO, deriv=2)
# 
# plot(predict_PIPO.prime.2)
# abline(h= 0, col="red", lty=2)
# 
# stress_week_PIPO <- uniroot.all(approxfun(predict_PIPO.prime.2$x, 
#                       predict_PIPO.prime.2$y),
#             interval = range(predict_PIPO.prime.2$x))
# 
# #PIED
# smooth_PIED <- smooth.spline(x=Phase1_Data_Weight_PIED$Week, 
#                              y=Phase1_Data_Weight_PIED$Weight_Est)
# predict_PIED <- predict(smooth_PIED)
# predict_PIED.prime.2 <- predict(smooth_PIED, deriv=2)
# 
# 
# plot(predict_PIED.prime.2)
# abline(h= 0, col="red", lty=2)
# 
# stress_week_PIED <- uniroot.all(approxfun(predict_PIED.prime.2$x, 
#                                           predict_PIED.prime.2$y),
#                                 interval = range(predict_PIED.prime.2$x))
# 
# #PIFL
# smooth_PIFL <- smooth.spline(x=Phase1_Data_Weight_PIFL$Week, 
#                              y=Phase1_Data_Weight_PIFL$Weight_Est)
# predict_PIFL <- predict(smooth_PIFL)
# predict_PIFL.prime.2 <- predict(smooth_PIFL, deriv=2)
# 
# 
# plot(predict_PIFL.prime.2)
# abline(h= 0, col="red", lty=2)
# 
# stress_week_PIFL <- uniroot.all(approxfun(predict_PIFL.prime.2$x, 
#                                           predict_PIFL.prime.2$y),
#                                 interval = range(predict_PIFL.prime.2$x))
# 
# #PSME
# smooth_PSME <- smooth.spline(x=Phase1_Data_Weight_PSME$Week, 
#                              y=Phase1_Data_Weight_PSME$Weight_Est)
# predict_PSME <- predict(smooth_PSME)
# predict_PSME.prime.2 <- predict(smooth_PSME, deriv=2)
# 
# 
# plot(predict_PSME.prime.2)
# abline(h= 0, col="red", lty=2)
# 
# stress_week_PSME <- uniroot.all(approxfun(predict_PSME.prime.2$x, 
#                                           predict_PSME.prime.2$y),
#                                 interval = range(predict_PSME.prime.2$x))
# 
# #PIEN
# smooth_PIEN <- smooth.spline(x=Phase1_Data_Weight_PIEN$Week, 
#                              y=Phase1_Data_Weight_PIEN$Weight_Est)
# predict_PIEN <- predict(smooth_PIEN)
# predict_PIEN.prime.2 <- predict(smooth_PIEN, deriv=2)
# 
# 
# plot(predict_PIEN.prime.2)
# abline(h= 0, col="red", lty=2)
# 
# stress_week_PIEN <- uniroot.all(approxfun(predict_PIEN.prime.2$x, 
#                                           predict_PIEN.prime.2$y),
#                                 interval = range(predict_PIEN.prime.2$x))
# 
# #combine data
# stress_week_all <- data.frame(stress_week_PIPO,
#                                stress_week_PIED,
#                                stress_week_PIFL,
#                                stress_week_PSME,
#                                stress_week_PIEN)
# names(stress_week_all) <- c("PIPO","PIED","PIFL","PSME","PIEN")
# stress_week <- slice(stress_week_all, 2)
# stress_week <- gather(stress_week, "Species", "Week")
# 
# 
# #save csv
# write.csv(stress_week, "data_analysis/stress_week.csv", quote = FALSE, row.names = FALSE)
# stress_week <- read.csv("data_analysis/stress_week.csv")
# #########
# 
# 


