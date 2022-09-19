#old analysis porometer
#find asymptote
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-09-03
#Last updated: 2022-09-03


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
