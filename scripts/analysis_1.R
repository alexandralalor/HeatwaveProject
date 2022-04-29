#baby analysis


#load tidyverse
library(tidyverse)
install.packages("ggfortify")
install.packages("ranger")
library(survival)
library(ggplot2)
library(ggfortify)
library(ranger)
install.packages("magick")
library(magick)

head(Phase1_meta)

image_read()


#make dead column logical
#working
Phase1_meta$Dead <- ifelse(Phase1_meta$Dead=="dead",1,0)

Phase1_meta

#temp
Phase1_meta_temp$Dead <- ifelse(Phase1_meta_temp$Dead=="dead",1,0)


#I want to make a mortality data
#lets put weeks on the x axis
#we want treatments separated
#we want droughted plants
?plot

plot(x=Phase1_meta$Week,
     y=Phase1_meta$Dead)
?ggplot


plot <- ggplot(Phase1_meta, 
               aes(x=Week,
                   y=Dead,
                   color=Species,
                   group=SpeciesID))


#Kaplan Meier Survival Curve
km <- with(Phase1_meta, Surv(Week, Dead))

km_fit <- survfit(Surv(Week, Dead)~1, data=Phase1_meta)
summary(km_fit, )
  
km_trt_fit <- survfit(Surv(Week, Dead)~Species, data=Phase1_meta)
autoplot(km_trt_fit)  


#TEMP Kaplan Meier Survival Curve
str(Phase1_meta_temp)
str(Phase1_meta)

Phase1_meta_temp <- Phase1_meta_temp %>% 
  mutate(across(Week, as.double))

Phase1_meta_temp$Treat <- str_c(Phase1_meta_temp$Species, "_", Phase1_meta_temp$Treatment_temp)
Phase1_meta_temp

km <- with(Phase1_meta_temp, Surv(Week, Dead))

km_fit <- survfit(Surv(Week, Dead)~1, data=Phase1_meta_temp)
summary(km_fit, )

km_trt_fit <- survfit(Surv(Week, Dead)~Treat, data=Phase1_meta_temp)
autoplot(km_trt_fit) +
  scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Paired") +
  theme_bw()

