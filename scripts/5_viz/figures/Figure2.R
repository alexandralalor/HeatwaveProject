#boxplot - Figure 2
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-10-01
#Last updated: 2022-10-01


#load packages
library(tidyverse)
library(ggplot2)
library(survminer) # publication quality graphs


###################################################################################
#read csv
Phase1_Data_All <- read_csv("data_analysis/Phase1_Data_All.csv")

#add legend column
Phase1_Data_All <- Phase1_Data_All %>%
  mutate(Legend = ScientificName)
Phase1_Data_All$Legend <- as.factor(Phase1_Data_All$Legend)
Phase1_Data_All <-
  transform(Phase1_Data_All, Legend = factor(Legend, levels = c("Pinus flexilis",
                                                                "Pseudotsuga menziesii",
                                                                "Picea engelmannii",
                                                                "Pinus edulis",
                                                                "Pinus ponderosa")))
levels(Phase1_Data_All$Legend)

#boxplot
Phase1_Data_All %>% 
  group_by(Species, Treatment_temp) %>% 
  arrange(Dead_Week) %>% 
  ggplot(aes(x = Dead_Week,
             y = Legend,
             fill = Treatment_temp)) +
  geom_boxplot() +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0 , 40, by = 2)) +
  xlab("Weeks to Mortality") +
  annotate("rect",
           xmin = 7, xmax = 8,
           ymin = 0, ymax = 5.25,
           color = "red",
           linetype = "dashed",
           size = 0.4,
           fill = "red",
           alpha = 0.3) +
  geom_text(label = "heatwave",
            x = 7.5, y = 5.5, color = "red", size = 6, family = "serif") +
  annotate("segment",
           x = 36, xend = 36,
           y = 0.5, yend = 1.3,
           color = "gray50", linetype = "solid", size = 0.6) +
  geom_text(label = "0.018*",
            x = 38, y = 1, color = "gray50", size = 6, family = "serif") +
  annotate("segment",
           x = 36, xend = 36,
           y = 1.6, yend = 2.5,
           color = "gray50", linetype = "solid", size = 0.6) +
  geom_text(label = "0.005**",
            x = 38.3, y = 2, color = "gray50", size = 6, family = "serif") +
  labs(fill = "") +
  #labs(caption = "\nFIGURE 2 | Heatwave Differences of Droughted Juveniles.\nHalf the droughted individuals (n = 20 per species) were exposed to a week long heatwave\n(indicated by the vertical red box). Boxplots show median time to mortality (rather than the mean). \nGrey bars on the right hand side show p-values of species which are significantly different in their \nmean time to mortality (two-sample t-test, p < 0.05).") +
  scale_fill_discrete(direction = -1,
                      labels = c("Drought Treatment", "Drought + Heatwave Treatment")) +
  theme_pubclean() +
  theme(text = element_text(family = "serif", size = 20),
        axis.text = element_text(size = 20),
        axis.text.y = element_text(face = "italic"),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        plot.caption = element_text(hjust = 0,
                                    family = "serif",
                                    #face = "bold",
                                    size = 20))
