#Data analysis - porometer
#find asymptote
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-09-03
#Last updated: 2022-09-03

#load packages
library(tidyverse)

#read CSVs
Phase1_Data_Porometer <- read_csv("data_QAQC/Phase1_Data_Porometer.csv")

#check out data
#glimpse(Phase1_Data_Porometer)

#convert variables
#if needed, convert time from double <dbl> to character <chr>
Phase1_Data_Porometer <- Phase1_Data_Porometer %>% 
  mutate(Time = ifelse(Time < 1000 & Time > 30, paste0("0", Phase1_Porometer$Time), 
                       ifelse(Time == 30, paste0("00", Phase1_Porometer$Time), 
                              ifelse(Time == 0, paste0("000", Phase1_Porometer$Time), Time))))
Phase1_Data_Porometer$DateTime <- as.POSIXct(Phase1_Data_Porometer$DateTime, format = "%m/%d/%Y %H:%M")
Phase1_Data_Porometer$Date <- as.Date(Phase1_Data_Porometer$Date, format = "%m/%d/%Y")

Phase1_Data_Porometer$Phase <- as.factor(Phase1_Data_Porometer$Phase)
Phase1_Data_Porometer$Chamber <- as.factor(Phase1_Data_Porometer$Chamber)
Phase1_Data_Porometer$ScientificName <- as.factor(Phase1_Data_Porometer$ScientificName)
Phase1_Data_Porometer$CommonName <- as.factor(Phase1_Data_Porometer$CommonName)
Phase1_Data_Porometer$Species <- as.factor(Phase1_Data_Porometer$Species)
Phase1_Data_Porometer$Treatment_temp <- as.factor(Phase1_Data_Porometer$Treatment_temp)
Phase1_Data_Porometer$Treatment_water <- as.factor(Phase1_Data_Porometer$Treatment_water)
Phase1_Data_Porometer$PorometerSubset <- as.factor(Phase1_Data_Porometer$PorometerSubset)
Phase1_Data_Porometer$Dead <- as.factor(Phase1_Data_Porometer$Dead)
Phase1_Data_Porometer$Heatwave_graph <- as.factor(Phase1_Data_Porometer$Heatwave_graph)
Phase1_Data_Porometer$Heatwave <- as.factor(Phase1_Data_Porometer$Heatwave)


################################################################################
# Stress Weeks
################################################################################

#new df
Phase1_Data_Porometer_test <- Phase1_Data_Porometer


#create stress levels (yes, no, maybe) based on porometer reading
Phase1_Data_Porometer_test <- Phase1_Data_Porometer_test %>% 
  mutate(Stress_Level = ifelse(Porometer_Est < 100 & Week > 1, "yes",
                               ifelse(Porometer_Est < 200 & Porometer_Est >= 100 & Week > 1, "maybe", "no")))

# identify threshold points for each stress level
Phase1_Data_Porometer_test_add_1 <- Phase1_Data_Porometer_test %>% 
  filter(Stress_Level == "no") %>% 
  group_by(SpeciesID) %>% 
  summarize(Stress_Week_no = max(Week))

Phase1_Data_Porometer_test_add_2 <- Phase1_Data_Porometer_test %>% 
  filter(Stress_Level == "yes") %>% 
  group_by(SpeciesID) %>% 
  summarize(Stress_Week_yes = min(Week))

Phase1_Data_Porometer_test_add_3 <- Phase1_Data_Porometer_test %>% 
  filter(Stress_Level == "maybe", Week != 20, Week != 21) %>% 
  group_by(SpeciesID) %>% 
  summarize(Stress_Week_maybe = max(Week))

Phase1_Data_Porometer_test_add_4 <- merge(Phase1_Data_Porometer_test_add_1, Phase1_Data_Porometer_test_add_2, all = T)
Phase1_Data_Porometer_test_add <- merge(Phase1_Data_Porometer_test_add_3, Phase1_Data_Porometer_test_add_4, all = T)
Phase1_Data_Porometer_test <- merge(Phase1_Data_Porometer_test, Phase1_Data_Porometer_test_add, all = T)


# Refine stress weeks.
# Plant should not be stressed if future readings show 200+ porometer
# Plant might be stressed if future readings show 100-200 porometer
# Plant is stressed if future readings show less than 100 porometer
Phase1_Data_Porometer_test_add_5 <- Phase1_Data_Porometer_test %>% 
  filter(Treatment_water == "Drought") %>% 
  filter(Week != 20, Week != 21) %>% 
  mutate(Stress_1 = ifelse(Stress_Week_no >= Week, "no", 
                           ifelse(Stress_Week_maybe >= Week, "maybe", "yes")))

Phase1_Data_Porometer_test_add_6 <- Phase1_Data_Porometer_test %>% 
  filter(Treatment_water == "Drought") %>% 
  filter(Week == 20 | Week == 21) %>% 
  mutate(Stress_1 = "yes")

Phase1_Data_Porometer_test_add_7 <- Phase1_Data_Porometer_test %>% 
  filter(Treatment_water == "Watered") %>% 
  mutate(Stress_1 = ifelse(Stress_Week_no >= Week, "no", 
                           ifelse(Stress_Week_maybe >= Week, "maybe", "yes")))

Phase1_Data_Porometer_test_add_8 <- merge(Phase1_Data_Porometer_test_add_5, Phase1_Data_Porometer_test_add_6, all = T)
Phase1_Data_Porometer_test_add <- merge(Phase1_Data_Porometer_test_add_7, Phase1_Data_Porometer_test_add_8, all = T)
Phase1_Data_Porometer_test <- merge(Phase1_Data_Porometer_test, Phase1_Data_Porometer_test_add, all = T)

Phase1_Data_Porometer_test <- Phase1_Data_Porometer_test %>% 
  mutate(Stress_2 = ifelse(is.na(Stress_1), "yes", Stress_1))


#manual transitions that I can't figure out
Phase1_Data_Porometer_test <- Phase1_Data_Porometer_test %>% 
  mutate(Stress_3 = ifelse(SpeciesID == "PIED33" & Week >= 5, "yes", 
                           ifelse(SpeciesID == "PIPO33" & Week >= 5, "yes", 
                                  ifelse(SpeciesID == "PIED03" & Week >= 9, "yes", Stress_2))))

#find stress point
# isolate 
Phase1_Data_Porometer_test_add_9 <- Phase1_Data_Porometer_test %>%
  filter(Stress_3 == "yes") %>%
  group_by(SpeciesID) %>%
  summarize(Stress_Week = min(Week))

Phase1_Data_Porometer_test <- merge(Phase1_Data_Porometer_test, Phase1_Data_Porometer_test_add_9, all = T)


# Add to all data
Phase1_Data_Porometer_add <- Phase1_Data_Porometer_test %>% 
  mutate(Stress_Level = Stress_3) %>% 
  select(c("SpeciesID", "Week", "Stress_Level", "Stress_Week"))
  
Phase1_Data_Porometer_1 <- merge(Phase1_Data_Porometer, Phase1_Data_Porometer_add, all = T)


#reorder and rearrange columns
Phase1_Data_Porometer_1 <- Phase1_Data_Porometer_1[, c(3,4,5,6,7,1,8,9,10,11,12,13,14,2,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33)]
Phase1_Data_Porometer <- Phase1_Data_Porometer_1 %>% 
  group_by(Species) %>% 
  arrange(SpeciesID, Week)

#save as csv
write.csv(Phase1_Data_Porometer, "data_analysis/Phase1_Data_Porometer.csv", quote = FALSE, row.names = FALSE)


###############################################################################
# Other Calculations
###############################################################################
#read csv
Phase1_Data_Porometer <- read_csv("data_analysis/Phase1_Data_Porometer.csv")

# Average stress week by species and treatment
# Remember that watered trees don't have a stress week
Phase1_Data_Porometer_add <- Phase1_Data_Porometer %>% 
  filter(Treatment_water == "Drought") %>% 
  group_by(Heatwave_graph) %>% 
  mutate(Stress_Week_Avg = round(mean(Stress_Week, na.rm = TRUE), digits = 1))

Phase1_Data_Porometer <- merge(Phase1_Data_Porometer, Phase1_Data_Porometer_add, all = T)

# SD
Phase1_Data_Porometer <- Phase1_Data_Porometer %>% 
  group_by(Species, Week, Treatment_temp, Treatment_water) %>%
  mutate(SD = sd(Porometer_Est, na.rm = T))

#save as csv
write.csv(Phase1_Data_Porometer, "data_analysis/Phase1_Data_Porometer.csv", quote = FALSE, row.names = FALSE)


################################################################################
# validation
################################################################################

#check 
# PIED---
# PIED03 (fixed)
# PIED33 (fixed)
# PIED49 (fixed)
# PIPO---
# PIPO33 (fixed)
# PIFL---
# PSME---
# PIEN---

Phase1_Data_Porometer_filter <- Phase1_Data_Porometer_test %>% 
  filter(SpeciesID == "PIPO33")

#Weight over time (averaged)
Phase1_Data_Porometer_filter %>% 
  filter(SpeciesID == "PIPO33") %>% 
  ggplot(aes(x = Week,
             y = Porometer,
             color = Stress_3)) +
  geom_point() +
  annotate("segment",
           x = 7, xend = 7,
           y = 0, yend = 600,
           color = "red",
           linetype = "dashed",
           size = 0.6) +
  annotate("segment",
           x = Phase1_Data_Porometer_filter$Stress_Week,
           xend = Phase1_Data_Porometer_filter$Stress_Week,
           y = 0, yend = 600,
           color = "black",
           linetype = "dashed",
           size = 0.6) +
  ylim(0, 600) +
  xlim(0,36) +
  #facet_wrap(~CommonName) +
  xlab("Week") +
  ylab("Stomatal Conductance") +
  labs(title = "Stomatal Conductance of Droughted Trees") +
  theme_minimal()
#theme(legend.position = "none")



################################################################################
#Find asymptote
################################################################################

# #filter porometer data, exclude NAs, treatment = drought
# Phase1_Data_Porometer <- Phase1_Data_Porometer %>% 
#   filter(!is.na(Porometer), Treatment_water == "Drought")
# 
# plot(x = Phase1_Data_Porometer$Week,
#      y = Phase1_Data_Porometer$Porometer)
# 
# Phase1_Data_Porometer_PIPO <- Phase1_Data_Porometer %>% 
#   filter(Species == "PIPO")
# Phase1_Data_Porometer_PIED <- Phase1_Data_Porometer %>% 
#   filter(Species == "PIED")
# Phase1_Data_Porometer_PIFL <- Phase1_Data_Porometer %>% 
#   filter(Species == "PIFL")
# Phase1_Data_Porometer_PSME <- Phase1_Data_Porometer %>% 
#   filter(Species == "PSME")
# Phase1_Data_Porometer_PIEN <- Phase1_Data_Porometer %>% 
#   filter(Species == "PIEN")
# 
# plot(x = Phase1_Data_Porometer_PIPO$Week,
#      y = Phase1_Data_Porometer_PIPO$Porometer)
# abline(h = c(100,90,75))
# plot(x = Phase1_Data_Porometer_PIED$Week,
#      y = Phase1_Data_Porometer_PIED$Porometer)
# abline(h = c(100,90,75))
# plot(x = Phase1_Data_Porometer_PIFL$Week,
#      y = Phase1_Data_Porometer_PIFL$Porometer)
# abline(h = c(100,90,75))
# plot(x = Phase1_Data_Porometer_PSME$Week,
#      y = Phase1_Data_Porometer_PSME$Porometer)
# abline(h = c(100,90,75))
# plot(x = Phase1_Data_Porometer_PIEN$Week,
#      y = Phase1_Data_Porometer_PIEN$Porometer)
# abline(h = c(100,90,75))


################################################################################
# other tests

# smooth <- smooth.spline(x = Phase1_Data_Porometer$Week,
#                         y = Phase1_Data_Porometer$Porometer)
# smooth_PIPO <- smooth.spline(x = Phase1_Data_Porometer_PIPO$Week,
#                              y = Phase1_Data_Porometer_PIPO$Porometer)
# smooth_PIED <- smooth.spline(x = Phase1_Data_Porometer_PIED$Week,
#                              y = Phase1_Data_Porometer_PIED$Porometer)
# smooth_PIFL <- smooth.spline(x = Phase1_Data_Porometer_PIFL$Week,
#                              y = Phase1_Data_Porometer_PIFL$Porometer)
# smooth_PSME <- smooth.spline(x = Phase1_Data_Porometer_PSME$Week,
#                              y = Phase1_Data_Porometer_PSME$Porometer)
# smooth_PIEN <- smooth.spline(x = Phase1_Data_Porometer_PIEN$Week,
#                              y = Phase1_Data_Porometer_PIEN$Porometer)
# 
# plot(smooth_PIPO)
# abline(h = c(100,90,75))
# plot(smooth_PIED)
# abline(h = c(100,90,75))
# plot(smooth_PIFL)
# abline(h = c(100,90,75))
# plot(smooth_PSME)
# abline(h = c(100,90,75))
# plot(smooth_PIEN)
# abline(h = c(100,90,75))
# plot(smooth)
# abline(h = c(100,90,75))


# predict_d2 <- predict(smooth_PIPO, deriv = 2)
# plot(predict)
# stress_week_1 <- as.matrix(uniroot.all(approxfun(predict_d2$x, predict_d2$y),
#                                        interval = range(predict_d2$x)))
# 
# 
# smooth <- smooth.spline(x = Phase1_Data_Weight_filter$Week,
#                         y = Phase1_Data_Weight_filter$Weight_g)
# predict_d2 <- predict(smooth, deriv=2)
# stress_week_1 <- as.matrix(uniroot.all(approxfun(predict_d2$x, predict_d2$y),
#                                        interval = range(predict_d2$x)))
# colnames(stress_week_1) <- ID
# stress_week <- merge(stress_week, stress_week_1, by = 0, all = T)
# stress_week <- stress_week %>% 
#   select(c(-"Row.names"))
# 
# 
# 
# 
# 
# log <- log(Phase1_Data_Porometer$Porometer)
# exp <- exp(Phase1_Data_Porometer$Porometer)
# plot(exp)
# plot(smooth_PIPO)
# lines(smooth, col = 2)
# ?plot
# predict(smooth)
# ?predict
# ?exp
# 
# 
# #############################
# fit <- nls(y ~ SSasymp(t, yf, y0, log_alpha), data = Phase1_Data_Porometer_PIPO)
# ?nls
# nls(smooth, data = Phase1_Data_Porometer)
# fo2 <- y ~ exp(a + b * log(x+1))
# fm2 <- nls(fo2, data = Phase1_Data_Porometer, start = list(a = 1, b = 1))
# 
# 
