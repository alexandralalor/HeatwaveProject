#Data analysis - weights
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-08-27
#Last updated: 2022-08-27

#load tidyverse
library(tidyverse)
library(knitr) #for kable
library(ggplot2)
library(rootSolve) #find where y crosses 0
library(nlme) #from mixed models lecture
library(lmerTest) #from mixed models lecture
library(forcats) #from mixed models lecture
#library(cobs) #for conreg, concave/convex package

#read CSVs
Phase1_Data_Weight <- read_csv("data_QAQC/Phase1_Data_Weight.csv")
Phase1_Data_Weight_Avg <- read_csv("data_QAQC/Phase1_Data_Weight_Avg.csv")

#filter data
Phase1_Data_Weight <- Phase1_Data_Weight %>% 
  filter(!is.na(Weight_Est), Treatment_water == "Drought")

#separate species
Phase1_Data_Weight_Avg_PIPO <- Phase1_Data_Weight %>% 
  filter(Species == "PIPO")
Phase1_Data_Weight_Avg_PIED <- Phase1_Data_Weight %>% 
  filter(Species == "PIED")
Phase1_Data_Weight_Avg_PIFL <- Phase1_Data_Weight %>% 
  filter(Species == "PIFL")
Phase1_Data_Weight_Avg_PSME <- Phase1_Data_Weight %>% 
  filter(Species == "PSME")
Phase1_Data_Weight_Avg_PIEN <- Phase1_Data_Weight %>% 
  filter(Species == "PIEN")

#ok i want to find the 2nd derivative of avg weights of each species

# x = week
# y = weight
# as x changes week to week, y is also changing. We want to find the change in
# weight over each week interval (1st derivative) and fit to a function
# Then, we take the 2nd derivative to find when that change switches from + to -

#PIPO
spl_avg_PIPO <- smooth.spline(x=Phase1_Data_Weight_Avg_PIPO$Week, 
                              y=Phase1_Data_Weight_Avg_PIPO$Weight_Est)
pred_avg_PIPO <- predict(spl_avg_PIPO)
pred_avg_PIPO.prime.1 <- predict(spl_avg_PIPO, deriv=1)
pred_avg_PIPO.prime.2 <- predict(spl_avg_PIPO, deriv=2)

plot(pred_avg_PIPO)

plot(pred_avg_PIPO.prime.1)

plot(pred_avg_PIPO.prime.2)
lines(pred_avg_PIPO.prime.2)
abline(h= 0, col="red", lty=2)

uniroot.all(approxfun(pred_avg_PIPO.prime.2$x, 
                      pred_avg_PIPO.prime.2$y),
            interval = range(pred_avg_PIPO.prime.2$x))

PIPO_derivative <- kable(uniroot.all(approxfun(pred_avg_PIPO.prime.2$x, 
                                               pred_avg_PIPO.prime.2$y), 
                                     interval = range(pred_avg_PIPO.prime.2$x)))


#PIED
spl_avg_PIED <- smooth.spline(x=Phase1_Data_Weight_Avg_PIED$Week, 
                              y=Phase1_Data_Weight_Avg_PIED$PercentWater)
pred_avg_PIED <- predict(spl_avg_PIED)
pred_avg_PIED.prime.1 <- predict(spl_avg_PIED, deriv=1)
pred_avg_PIED.prime.2 <- predict(spl_avg_PIED, deriv=2)

pred_avg_PIED
plot(pred_avg_PIED)

pred_avg_PIED.prime.1
plot(pred_avg_PIED.prime.1)

pred_avg_PIED.prime.2
plot(pred_avg_PIED.prime.2)
lines(pred_avg_PIED.prime.2)
abline(h= 0, col="red", lty=2)

PIED_derivative <- kable(uniroot.all(approxfun(pred_avg_PIED.prime.2$x, 
                                               pred_avg_PIED.prime.2$y), 
                                     interval = range(pred_avg_PIED.prime.2$x)))


#PIFL
spl_avg_PIFL <- smooth.spline(x=Phase1_Data_Weight_Avg_PIFL$Week, 
                              y=Phase1_Data_Weight_Avg_PIFL$PercentWater)
pred_avg_PIFL <- predict(spl_avg_PIFL)
pred_avg_PIFL.prime.1 <- predict(spl_avg_PIFL, deriv=1)
pred_avg_PIFL.prime.2 <- predict(spl_avg_PIFL, deriv=2)

pred_avg_PIFL
plot(pred_avg_PIFL)

pred_avg_PIFL.prime.1
plot(pred_avg_PIFL.prime.1)

pred_avg_PIFL.prime.2
plot(pred_avg_PIFL.prime.2)
lines(pred_avg_PIFL.prime.2)
abline(h= 0, col="red", lty=2)

PIFL_derivative <- kable(uniroot.all(approxfun(pred_avg_PIFL.prime.2$x, 
                                               pred_avg_PIFL.prime.2$y), 
                                     interval = range(pred_avg_PIFL.prime.2$x)))

#PSME
spl_avg_PSME <- smooth.spline(x=Phase1_Data_Weight_Avg_PSME$Week, 
                              y=Phase1_Data_Weight_Avg_PSME$PercentWater)
pred_avg_PSME <- predict(spl_avg_PSME)
pred_avg_PSME.prime.1 <- predict(spl_avg_PSME, deriv=1)
pred_avg_PSME.prime.2 <- predict(spl_avg_PSME, deriv=2)

pred_avg_PSME
plot(pred_avg_PSME)

pred_avg_PSME.prime.1
plot(pred_avg_PSME.prime.1)

pred_avg_PSME.prime.2
plot(pred_avg_PSME.prime.2)
lines(pred_avg_PSME.prime.2)
abline(h= 0, col="red", lty=2)

PSME_derivative <- kable(uniroot.all(approxfun(pred_avg_PSME.prime.2$x, 
                      pred_avg_PSME.prime.2$y), 
            interval = range(pred_avg_PSME.prime.2$x)))


#PIEN
spl_avg_PIEN <- smooth.spline(x=Phase1_Data_Weight_Avg_PIEN$Week, 
                              y=Phase1_Data_Weight_Avg_PIEN$PercentWater)
pred_avg_PIEN <- predict(spl_avg_PIEN)
pred_avg_PIEN.prime.1 <- predict(spl_avg_PIEN, deriv=1)
pred_avg_PIEN.prime.2 <- predict(spl_avg_PIEN, deriv=2)

pred_avg_PIEN
plot(pred_avg_PIEN)

pred_avg_PIEN.prime.1
plot(pred_avg_PIEN.prime.1)

pred_avg_PIEN.prime.2
plot(pred_avg_PIEN.prime.2)
lines(pred_avg_PIEN.prime.2)
abline(h= 0, col="red", lty=2)

PIEN_derivative <- kable(uniroot.all(approxfun(pred_avg_PIEN.prime.2$x, 
                      pred_avg_PIEN.prime.2$y), 
            interval = range(pred_avg_PIEN.prime.2$x)))





#############

#not separated by species, not working so well
spl_avg <- smooth.spline(x=Phase1_Data_Weight_Avg$Week, y=Phase1_Data_Weight_Avg$Weight_Est)
pred_avg <- predict(spl_avg)
pred_avg.prime.1 <- predict(spl_avg, deriv=1)
pred_avg.prime.2 <- predict(spl_avg, deriv=2)

pred_avg
plot(pred_avg)
lines(pred_avg, col=2)

pred_avg.prime.1
plot(pred_avg.prime.1)

pred_avg.prime.2
plot(pred_avg.prime.2)
abline(h= 0, col="red", lty=2)
lines(pred_avg.prime.2)



#Weight over time (averaged)
Phase1_Data_Weight_Avg %>% 
  filter(Treatment_water == "Drought", Species == "PIEN", !is.na(Weight_Est)) %>%
  group_by(Species) %>%
  ggplot(aes(x = Week,
             y = Weight_Est,
             color = CommonName)) +
  geom_point() +
  geom_line() +
  # geom_errorbar(aes(x = Week, ymin=Weight_Est-SD, ymax=Weight_Est+SD),
  #              width=0.1, color='black', alpha = 0.5) +
  ylim(200, 800) +
  xlim(0,36) +
  geom_text(data = HW_label,
            aes(label = Heatwave),
            x = 11, y = 800,
            color = "red",
            size = 2.8) +
  # geom_rect(data = HW_rect,
  #           aes(xmin = 7, xmax = 8,
  #               ymin = 200, ymax = 800),
  #           fill = "red",
  #           color = "red",
  #           alpha = 0.05) +
  annotate("segment",
           x = 7, xend = 7,
           y = 200, yend = 800,
           color = "red",
           linetype = "dashed",
           size = 0.6) +
  facet_wrap(~Treatment_temp) +
  xlab("Week") +
  ylab("Water Weight (g)") +
  labs(title = "Weight of Droughted Trees") +
  theme_minimal()
