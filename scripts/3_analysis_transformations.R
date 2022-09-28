
#load packages
library(tidyverse)
library(MASS) #boxcox
library(fitdistrplus) #weinbull distribution

#read csv
heatwave <- read.csv("data_analysis/Dead_Week.csv")

#for some reason need to make a function to round
rnd <- function(x) trunc(x+sign(x)*0.5)
heatwave <- heatwave %>% 
  mutate(Dead_Week = rnd(Dead_Week))

# #Transformations
#log
heatwave <- heatwave %>%
  group_by(Species, Treatment_temp) %>%
  mutate(Dead_Week_log = log(Dead_Week))
# 
# #square root
# heatwave <- heatwave %>% 
#   group_by(Species, Treatment_temp) %>% 
#   mutate(Dead_Week = sqrt(Dead_Week))
# 
# #cube root
# heatwave <- heatwave %>% 
#   group_by(Species, Treatment_temp) %>% 
#   mutate(Dead_Week = Dead_Week^(1/3))
# 
# #reciprocal
# heatwave <- heatwave %>% 
#   group_by(Species, Treatment_temp) %>% 
#   mutate(Dead_Week = 1/Dead_Week)


#PIPO
heatwave.pipo.amb <- heatwave %>% 
  filter(Species == "PIPO", Treatment_temp == "Ambient")
heatwave.pipo.hw <- heatwave %>% 
  filter(Species == "PIPO", Treatment_temp == "Ambient_HW")

#PIED
heatwave.pied.amb <- heatwave %>% 
  filter(Species == "PIED", Treatment_temp == "Ambient")
heatwave.pied.hw <- heatwave %>% 
  filter(Species == "PIED", Treatment_temp == "Ambient_HW")

#PIFL
heatwave.pifl.amb <- heatwave %>% 
  filter(Species == "PIFL", Treatment_temp == "Ambient")
heatwave.pifl.hw <- heatwave %>% 
  filter(Species == "PIFL", Treatment_temp == "Ambient_HW")

#PSME
heatwave.psme.amb <- heatwave %>% 
  filter(Species == "PSME", Treatment_temp == "Ambient")
heatwave.psme.hw <- heatwave %>% 
  filter(Species == "PSME", Treatment_temp == "Ambient_HW")

#PIEN
heatwave.pien.amb <- heatwave %>% 
  filter(Species == "PIEN", Treatment_temp == "Ambient")
heatwave.pien.hw <- heatwave %>% 
  filter(Species == "PIEN", Treatment_temp == "Ambient_HW")


fw <- fitdist(heatwave.pipo.amb$Dead_Week, "weibull")
summary(fw)
hist(fw$)
qqnorm(fw$data)
?fitdist

#PIPO
hist(heatwave.pipo.amb$Dead_Week)
boxplot(heatwave.pipo.amb$Dead_Week)
qqnorm(heatwave.pipo.amb$Dead_Week)
qqline(heatwave.pipo.amb$Dead_Week)

hist(heatwave.pipo.hw$Dead_Week)
boxplot(heatwave.pipo.hw$Dead_Week)
qqnorm(heatwave.pipo.hw$Dead_Week)
qqline(heatwave.pipo.hw$Dead_Week)


#PIED
hist(heatwave.pied.amb$Dead_Week)
boxplot(heatwave.pied.amb$Dead_Week)
qqnorm(heatwave.pied.amb$Dead_Week)
qqline(heatwave.pied.amb$Dead_Week)

hist(heatwave.pied.hw$Dead_Week)
boxplot(heatwave.pied.hw$Dead_Week)
qqnorm(heatwave.pied.hw$Dead_Week)
qqline(heatwave.pied.hw$Dead_Week)


#PIFL
hist(heatwave.pifl.amb$Dead_Week)
boxplot(heatwave.pifl.amb$Dead_Week)
qqnorm(heatwave.pifl.amb$Dead_Week)
qqline(heatwave.pifl.amb$Dead_Week)

hist(heatwave.pifl.hw$Dead_Week)
boxplot(heatwave.pifl.hw$Dead_Week)
qqnorm(heatwave.pifl.hw$Dead_Week)
qqline(heatwave.pifl.hw$Dead_Week)


#PSME
hist(heatwave.psme.amb$Dead_Week)
boxplot(heatwave.psme.amb$Dead_Week)
qqnorm(heatwave.psme.amb$Dead_Week)
qqline(heatwave.psme.amb$Dead_Week)

hist(heatwave.psme.hw$Dead_Week)
boxplot(heatwave.psme.hw$Dead_Week)
qqnorm(heatwave.psme.hw$Dead_Week)
qqline(heatwave.psme.hw$Dead_Week)


#PIEN
hist(heatwave.pien.amb$Dead_Week)
boxplot(heatwave.pien.amb$Dead_Week)
qqnorm(heatwave.pien.amb$Dead_Week)
qqline(heatwave.pien.amb$Dead_Week)

hist(heatwave.pien.hw$Dead_Week)
boxplot(heatwave.pien.hw$Dead_Week)
qqnorm(heatwave.pien.hw$Dead_Week)
qqline(heatwave.pien.hw$Dead_Week)





#Shapiro-Wilk
shapiro.test(heatwave.pipo.amb$Dead_Week)
shapiro.test(heatwave.pipo.hw$Dead_Week)
shapiro.test(heatwave.pied.amb$Dead_Week)
shapiro.test(heatwave.pied.hw$Dead_Week)
shapiro.test(heatwave.pifl.amb$Dead_Week)
shapiro.test(heatwave.pifl.hw$Dead_Week)
shapiro.test(heatwave.psme.amb$Dead_Week)
shapiro.test(heatwave.psme.hw$Dead_Week)
shapiro.test(heatwave.pien.amb$Dead_Week)
shapiro.test(heatwave.pien.hw$Dead_Week)



# #Kolmogorov-Smirnov test
# cdf.pifl <- qqnorm(heatwave.pifl.amb$Dead_Week)
# ks.test(heatwave.pifl.amb$Dead_Week, cdf.pifl$x)
# 
# hist(cdf.pifl$y)
# boxplot(cdf.pipo$y)
# qqnorm(cdf.pied$x)
# qqline(cdf.pipo$x)


#box-cox
#ralph book
tdata <- boxcox(heatwave.pied.amb)

