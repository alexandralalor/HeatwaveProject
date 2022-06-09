#Data Viz Final Project
#To summarize data from all pixels. This means creating bins and grouping colors, as well as cleaning up some columns
#allielalor@email.arizona.edu
#First created: 2022-05-02
#Last updated: 2022-06-08

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
tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_August_26_2021.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_September_02_2021.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_September_09_2021.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_September_16_2021.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_September_24_2021.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_September_30_2021.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_October_07_2021.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_October_15_2021.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_October_21_2021.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_October_29_2021.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_November_05_2021.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_November_11_2021.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_November_19_2021.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_November_26_2021.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_December_03_2021.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_December_10_2021.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_December_17_2021.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_December_30_2021.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_January_06_2022.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_January_13_2022.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_January_21_2022.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_January_28_2022.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_February_04_2022.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_February_11_2022.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_February_18_2022.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_February_24_2022.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_March_03_2022.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_March_11_2022.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_March_18_2022.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_March_24_2022.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_March_31_2022.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_April_07_2022.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_April_15_2022.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_April_21_2022.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_April_29_2022.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_May_06_2022.csv")
# tree_rgb <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_May_12_2022.csv")

# tree_rgb_all <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_all.csv")

#combine all dates
# tree_rgb_all <- rbind(tree_rgb_August_26_2021, tree_rgb_November_11_2021, tree_rgb_November_19_2021,
#                       tree_rgb_November_5_2021, tree_rgb_October_15_2021, tree_rgb_October_21_2021,
#                       tree_rgb_October_29_2021, tree_rgb_October_7_2021, tree_rgb_September_16_2021,
#                       tree_rgb_September_2_2021, tree_rgb_September_24_2021, tree_rgb_September_30_2021,
#                       tree_rgb_September_9_2021)

#reducing the data frame

#bining
glimpse(tree_rgb)

#bin red
tree_rgb <- tree_rgb %>% 
  mutate(red_class = ifelse(red <= 10, "1-10", 
                            ifelse(red <= 20, "11-20", 
                                   ifelse(red <= 30, "21-30",
                                          ifelse(red <= 40, "31-40",
                                                 ifelse(red <= 50, "41-50",
                                                        ifelse(red <= 60, "51-60",
                                                               ifelse(red <= 70, "61-70",
                                                                      ifelse(red <= 80, "71-80",
                                                                             ifelse(red <= 90, "81-90",
                                                                                    ifelse(red <= 100, "91-100",
                                                                                           ifelse(red <= 110, "101-110",
                                                                                                  ifelse(red <= 120, "111-120",
                                                                                                         ifelse(red <= 130, "121-130",
                                                                                                                ifelse(red <= 140, "131-140",
                                                                                                                       ifelse(red <= 150, "141-150",
                                                                                                                              ifelse(red <= 160, "151-160",
                                                                                                                                     ifelse(red <= 170, "161-170",
                                                                                                                                            ifelse(red <= 180, "171-180",
                                                                                                                                                   ifelse(red <= 190, "181-190",
                                                                                                                                                          ifelse(red <= 200, "191-200",
                                                                                                                                                                 ifelse(red <= 210, "201-210",
                                                                                                                                                                        ifelse(red <= 220, "211-220",
                                                                                                                                                                               ifelse(red <= 230, "221-230",
                                                                                                                                                                                      ifelse(red <= 240, "231-240",
                                                                                                                                                                                             ifelse(red <= 250, "241-250", "250-255"))))))))))))))))))))))))))
##############################################################################################################################################################################################################################################################
#bin green
tree_rgb <- tree_rgb %>% 
  mutate(green_class = ifelse(green <= 10, "1-10", 
                            ifelse(green <= 20, "11-20", 
                                   ifelse(green <= 30, "21-30",
                                          ifelse(green <= 40, "31-40",
                                                 ifelse(green <= 50, "41-50",
                                                        ifelse(green <= 60, "51-60",
                                                               ifelse(green <= 70, "61-70",
                                                                      ifelse(green <= 80, "71-80",
                                                                             ifelse(green <= 90, "81-90",
                                                                                    ifelse(green <= 100, "91-100",
                                                                                           ifelse(green <= 110, "101-110",
                                                                                                  ifelse(green <= 120, "111-120",
                                                                                                         ifelse(green <= 130, "121-130",
                                                                                                                ifelse(green <= 140, "131-140",
                                                                                                                       ifelse(green <= 150, "141-150",
                                                                                                                              ifelse(green <= 160, "151-160",
                                                                                                                                     ifelse(green <= 170, "161-170",
                                                                                                                                            ifelse(green <= 180, "171-180",
                                                                                                                                                   ifelse(green <= 190, "181-190",
                                                                                                                                                          ifelse(green <= 200, "191-200",
                                                                                                                                                                 ifelse(green <= 210, "201-210",
                                                                                                                                                                        ifelse(green <= 220, "211-220",
                                                                                                                                                                               ifelse(green <= 230, "221-230",
                                                                                                                                                                                      ifelse(green <= 240, "231-240",
                                                                                                                                                                                             ifelse(green <= 250, "241-250", "250-255"))))))))))))))))))))))))))
################################################################################################################################################################################################################################################################
#bin blue
tree_rgb <- tree_rgb %>% 
  mutate(blue_class = ifelse(blue <= 10, "1-10", 
                            ifelse(blue <= 20, "11-20", 
                                   ifelse(blue <= 30, "21-30",
                                          ifelse(blue <= 40, "31-40",
                                                 ifelse(blue <= 50, "41-50",
                                                        ifelse(blue <= 60, "51-60",
                                                               ifelse(blue <= 70, "61-70",
                                                                      ifelse(blue <= 80, "71-80",
                                                                             ifelse(blue <= 90, "81-90",
                                                                                    ifelse(blue <= 100, "91-100",
                                                                                           ifelse(blue <= 110, "101-110",
                                                                                                  ifelse(blue <= 120, "111-120",
                                                                                                         ifelse(blue <= 130, "121-130",
                                                                                                                ifelse(blue <= 140, "131-140",
                                                                                                                       ifelse(blue <= 150, "141-150",
                                                                                                                              ifelse(blue <= 160, "151-160",
                                                                                                                                     ifelse(blue <= 170, "161-170",
                                                                                                                                            ifelse(blue <= 180, "171-180",
                                                                                                                                                   ifelse(blue <= 190, "181-190",
                                                                                                                                                          ifelse(blue <= 200, "191-200",
                                                                                                                                                                 ifelse(blue <= 210, "201-210",
                                                                                                                                                                        ifelse(blue <= 220, "211-220",
                                                                                                                                                                               ifelse(blue <= 230, "221-230",
                                                                                                                                                                                      ifelse(blue <= 240, "231-240",
                                                                                                                                                                                             ifelse(blue <= 250, "241-250", "250-255"))))))))))))))))))))))))))
###############################################################################################################################################################################################################################################################
#summarize by bins
tree_rgb_sum <- tree_rgb %>% 
  group_by(Week, Date, Species, SpeciesID, Treatment_temp, Treatment_water, red_class, green_class, blue_class) %>% 
  summarize(red = round(mean(red)),
            green = round(mean(green)),
            blue = round(mean(blue)),
            col_freq = sum(col_freq))


#fill in summary df
#add hex codes to summary df
hex <- as.data.frame(rgb2hex(r = tree_rgb_sum$red, 
                             g = tree_rgb_sum$green, 
                             b = tree_rgb_sum$blue))
colnames(hex) <- "col_hex"
tree_rgb_sum <- cbind(tree_rgb_sum, hex)
#reorder columns
tree_rgb_sum <- tree_rgb_sum[, c(1,2,3,4,5,6,7,8,9,10,11,12,14,13)]
#calculate total # pixels and percent of each color, add to summary df
tree_rgb_sum <- tree_rgb_sum %>% 
  group_by(Week, Date, Species, SpeciesID, Treatment_temp, Treatment_water) %>% 
  mutate(col_total = sum(col_freq)) %>% 
  mutate(col_share = round(100*(col_freq/col_total), digits = 1))

#Remember to change date when saving!
#write csv
write.csv(tree_rgb_sum, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb_sum/tree_rgb_sum_August_26_2021.csv", quote=FALSE, row.names = FALSE)


