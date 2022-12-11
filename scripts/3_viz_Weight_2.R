#Data viz - weights
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-12-10
#Last updated: 2022-12-10

#load packages
library(tidyverse)

#read csvs
Phase1_Data_All <- read_csv("data_analysis/Phase1_Data_All.csv")
#Phase1_Data_Weight  <- read_csv("data_analysis/Phase1_Data_Weight.csv")
#Phase1_Data_Weight_Avg  <- read_csv("data_analysis/Phase1_Data_Weight_Avg.csv")

Phase1_Data_All_w <- Phase1_Data_All %>%
  mutate(Legend = ScientificName)
Phase1_Data_All_w$Legend <- as.factor(Phase1_Data_All_w$Legend)
Phase1_Data_All_w <-
  transform(Phase1_Data_All_w, Legend = factor(Legend, levels = c("Pinus ponderosa", "Pinus edulis", "Picea engelmannii", "Pseudotsuga menziesii", "Pinus flexilis")))
levels(Phase1_Data_All_w$Legend)

#filter data
Phase1_Data_Weight_graph <- Phase1_Data_All_w %>% 
  filter(Treatment_temp == "Ambient", Treatment_water == "Drought",
         !is.na(WaterWeight_Calc))


label <- Phase1_Data_Weight_graph %>% 
  group_by(Legend) %>% 
  summarize(Stress_Week_Avg = mean(Stress_Week_Avg_Weight))
str(label)
levels(label$Legend)

dat_text <- data.frame(
  cyl   = c(4, 6, 8, 4, 6, 8),
  am    = c(0, 0, 0, 1, 1, 1))

label <- data.frame(xPIPO = 4, xPIED = 2, xPIFL = 11, xPSME = 9, xPIEN = 8)

dat_text$label <- sprintf(
  "%s, %s cylinders",
  ifelse(dat_text$am == 0, "automatic", "manual"),
  dat_text$cyl
)

test <- data.frame(x = c(4.39, 4.32, 7.67, 7.82, 11.48),
                   Legend = factor(c("Pinus ponderosa","Pinus edulis", "Picea engelmannii", "Pseudotsuga menziesii", "Pinus flexilis"), 
                                   levels = c("Pinus ponderosa", "Pinus edulis", "Picea engelmannii", "Pseudotsuga menziesii", "Pinus flexilis")))

ann_text <- data.frame(mpg = 15,wt = 5,lab = "Text",
                       cyl = factor(8,levels = c("4","6","8")))
ann_line<-data.frame(xmid=366,xmin=310,xmax=420,y0=0,y2=2,y=1,
                     id=factor("orange",levels=c("apple","banana","orange")))

p1 + geom_segment(data=ann_line,aes(x=xmid,xend=xmin,y=y,yend=y),arrow=arrow(length=unit(0.2,"cm")),show_guide=F)+
  geom_segment(data=ann_line,
               aes(x=xmid,xend=xmax,y=y,yend=y),
               arrow=arrow(length=unit(0.2,"cm")),show_guide=F)+
  geom_segment(data=ann_line,aes(x=xmid,xend=xmid,y=y0,yend=y2),show_guide=F)+   
  geom_text(data=ann_text,aes(x=x,y=y,label=label,size=3),show_guide=F)



p + geom_text(data = ann_text,label = "Text")



#Graph
Phase1_Data_Weight_graph %>% 
  ggplot(aes(x = Week,
             y = WaterWeight_Calc,
             color = SpeciesID)) +
  geom_point() +
  geom_line() +
  geom_segment(data = test,
               aes(x = x, xend = x,
                   y = 0, yend = 500),
               color = "black",
               linetype = "dashed",
               size = 0.8) +
  ylim(0, 500) +
  xlim(0, 36) +
  facet_wrap(~Legend) +
  xlab("Weeks") +
  ylab("Water Weight (g)") +
  labs(caption = "FIGURE 3 | Water loss curves of droughted seedlings, grouped by species. Curves were calculated using individual weight data. Black dotted lines show the average inflection point among all curves to show where concavity changes from concave up to concave down, reflecting the time when water availability transitioned from a non-limiting to a limiting resource. ANOVA was performed by re-calculating individual time-to-mortality to start at this water stress point, comparing water stress tolerance across species.") +
  theme_minimal() +
  theme(legend.position="none",
        text = element_text(family = "serif"),
        plot.caption = element_text(hjust = 0,
                                    family = "serif",
                                    #face = "bold",
                                    size = 10))
