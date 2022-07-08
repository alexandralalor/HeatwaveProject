#kaplan meier survival curve analysis
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2022-06-13

#load packages
library(tidyverse)
library(survival)
library(ggplot2)
library(ggfortify)
# library(ranger)
# library(magick)

#read in data
Phase1_Plants <- read_csv("data_clean/Phase1_Plants.csv")
Phase1_InitialData <- read_csv("data_clean/Phase1_InitialData.csv")

#merge important info
Phase1_InitialData <- Phase1_InitialData %>% 
  select(-c(BiomassBag_g, Bag_g, Comments))

Phase1_Plants_All <- merge(Phase1_Plants, Phase1_InitialData, all = T)

#make dead column logical
Phase1_Plants_All$Dead <- ifelse(Phase1_Plants_All$Dead=="dead",1,0)

#create new heatwave variables for graphing
Phase1_Plants_All <- Phase1_Plants_All %>% 
  mutate(Heatwave_graph = Treatment_temp) %>% 
  separate(Heatwave_graph, sep = "_",
           into = c("Ambient", "Heatwave_graph")) %>% 
  mutate(Heatwave = Heatwave_graph)

Phase1_Plants_All$Heatwave[is.na(Phase1_Plants_All$Heatwave)] <- "no"
Phase1_Plants_All <- Phase1_Plants_All %>% 
  mutate(Heatwave = ifelse(Phase1_Plants_All$Heatwave == "HW", "yes", "no"))
Phase1_Plants_All <- Phase1_Plants_All %>% 
  mutate(Heatwave_graph = ifelse(Phase1_Plants_All$Heatwave_graph == "HW", "heatwave", Phase1_Plants_All$Heatwave_graph))

Phase1_Plants_All$Heatwave_graph <- str_c(Phase1_Plants_All$CommonName, "_", Phase1_Plants_All$Heatwave_graph)
Phase1_Plants_All$Heatwave_graph[is.na(Phase1_Plants_All$Heatwave_graph)] <- "X"
Phase1_Plants_All <- Phase1_Plants_All %>% 
  mutate(Heatwave_graph = ifelse(Phase1_Plants_All$Heatwave_graph == "X", Phase1_Plants_All$CommonName, Phase1_Plants_All$Heatwave_graph))


#Kaplan Meier Survival Curve - combined
km <- with(Phase1_Plants_All, Surv(Week, Dead))

km_fit <- survfit(Surv(Week, Dead)~1, data=Phase1_Plants_All)
summary(km_fit, )

km_trt_fit <- survfit(Surv(Week, Dead)~Species, data=Phase1_Plants_All)

autoplot(km_trt_fit)  


#Kaplan Meier Survival Curve - separated by treatment
km <- with(Phase1_Plants_All, Surv(Week, Dead))

km_fit <- survfit(Surv(Week, Dead)~1, data=Phase1_Plants_All)

km_trt_fit <- survfit(Surv(Week, Dead)~Heatwave_graph, data=Phase1_Plants_All)

autoplot(km_trt_fit) +
  scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Paired") +
  scale_x_continuous(breaks = seq(0 , 36, by = 4)) +
  annotate("segment",
           x = 7, xend = 7,
           y = 0, yend = 1,
           color = "red",
           linetype = "dashed",
           size = 0.8) +
  geom_text(label = "Heatwave",
            x = 5, y = 0.78, color = "red", size = 3) +
  xlab("Weeks") +
  ylab("Survivorship") +
  labs(title = "Kaplan Meier Survival Curve: Heatwave Effects by Species",
       color = "Species_Treatment", fill = "Species_Treatment") +
  theme_bw()

