#Data Viz Final Project
#To summarize data from all pixels. This is mainly creating bins and grouping colors, as well as cleaning up some columns
#allielalor@email.arizona.edu
#First created: 2022-05-02
#Last updated: 2022-05-02

#load libraries
library(tidyverse)
library(ggtern) #for rbg2hex
# library(colorfindr) #for get_colors
# library(sjmisc) #for rotate_df
# library(ggtern) #for rbg2hex
# library(countcolors) #for masking and reducing black colors to one point
# library(tools) #for file naming

################################################################################

#read in files
# tree_rgb_August_26_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_August_26_2021.csv")
# tree_rgb_November_11_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_November_11_2021.csv")
# tree_rgb_November_19_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_November_19_2021.csv")
# tree_rgb_November_5_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_November_5_2021.csv")
# tree_rgb_October_15_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_October_15_2021.csv")
# tree_rgb_October_21_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_October_21_2021.csv")
# tree_rgb_October_29_2021 <-read.csv("data_raw/final_project/tree_rgb/tree_rgb_October_29_2021.csv")
# tree_rgb_October_7_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_October_7_2021.csv")
# tree_rgb_September_16_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_September_16_2021.csv")
# tree_rgb_September_2_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_September_2_2021.csv")
# tree_rgb_September_24_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_September_24_2021.csv")
# tree_rgb_September_30_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_September_30_2021.csv")
# tree_rgb_September_9_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_September_9_2021.csv")

tree_rgb_all <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_all.csv")

#combine all dates
# tree_rgb_all <- rbind(tree_rgb_August_26_2021, tree_rgb_November_11_2021, tree_rgb_November_19_2021,
#                       tree_rgb_November_5_2021, tree_rgb_October_15_2021, tree_rgb_October_21_2021,
#                       tree_rgb_October_29_2021, tree_rgb_October_7_2021, tree_rgb_September_16_2021,
#                       tree_rgb_September_2_2021, tree_rgb_September_24_2021, tree_rgb_September_30_2021,
#                       tree_rgb_September_9_2021)

#reducing the data frame

#bining
# 8x8x8 = 526
# 8 blocks for each (rgb), each 32 pixels wide

#bin red
tree_rgb_all <- tree_rgb_all %>% 
  mutate(red_class = ifelse(red <= 32, "1-32", 
                            ifelse(red <= 64, "33-64", 
                                   ifelse(red <= 96, "65-96",
                                          ifelse(red <= 128, "97-128",
                                                 ifelse(red <= 160, "129-160",
                                                        ifelse(red <= 192, "161-192",
                                                               ifelse(red <= 224, "193-224", "225-256"))))))))
#bin green
tree_rgb_all <- tree_rgb_all %>% 
  mutate(green_class = ifelse(green <= 32, "1-32", 
                              ifelse(green <= 64, "33-64", 
                                     ifelse(green <= 96, "65-96",
                                            ifelse(green <= 128, "97-128",
                                                   ifelse(green <= 160, "129-160",
                                                          ifelse(green <= 192, "161-192",
                                                                 ifelse(green <= 224, "193-224", "225-256"))))))))
#bin blue
tree_rgb_all <- tree_rgb_all %>% 
  mutate(blue_class = ifelse(blue <= 32, "1-32", 
                             ifelse(blue <= 64, "33-64", 
                                    ifelse(blue <= 96, "65-96",
                                           ifelse(blue <= 128, "97-128",
                                                  ifelse(blue <= 160, "129-160",
                                                         ifelse(blue <= 192, "161-192",
                                                                ifelse(blue <= 224, "193-224", "225-256"))))))))
#summarize by bins
tree_rgb_sum_all <- tree_rgb_all %>% 
  group_by(Week, Date, Species, SpeciesID, Treatment_temp, Treatment_water, red_class, green_class, blue_class) %>% 
  summarize(red = round(mean(red)),
            green = round(mean(green)),
            blue = round(mean(blue)),
            col_freq = sum(col_freq))


#fill in summary df
#add hex codes to summary df
hex <- as.data.frame(rgb2hex(r = tree_rgb_sum_all$red, 
                             g = tree_rgb_sum_all$green, 
                             b = tree_rgb_sum_all$blue))
colnames(hex) <- "col_hex"
tree_rgb_sum_all <- cbind(tree_rgb_sum_all, hex)
#reorder columns
tree_rgb_sum_all <- tree_rgb_sum_all[, c(1,2,3,4,5,6,7,8,9,10,11,12,14,13)]
#calculate total # pixels and percent of each color, add to summary df
tree_rgb_sum_all <- tree_rgb_sum_all %>% 
  group_by(Week, Date, Species, SpeciesID, Treatment_temp, Treatment_water) %>% 
  mutate(col_total = sum(col_freq)) %>% 
  mutate(col_share = round(100*(col_freq/col_total), digits = 1))


#write csv
write.csv(tree_rgb_sum_all, "data_raw/final_project/tree_rgb_sum/tree_rgb_sum_all.csv", quote=FALSE, row.names = FALSE)


