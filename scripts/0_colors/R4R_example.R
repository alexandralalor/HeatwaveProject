#Roots for Resilience example
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-12-08
#Last updated: 2022-12-08

#load libraries
library(tidyverse)
library(colorfindr) #for get_colors
library(sjmisc)     #for rotate_df
library(ggtern)     #for rbg2hex
library(ggplot2)    #for graphing

###############################################################################################
# 1. Create data frame with important information related to photos, using photo file name
###############################################################################################

#load in images and name them based on file path
my_path_photos <- "Phase1_Photos/"
folder_names_list <- list.files(my_path_photos)
folder_names_list

#make empty files names data frame
file_names_df <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(file_names_df) <- c("Photos_folder", "Date", "Stage", 
                             "Species", "SpeciesID", "Treatment_temp","Treatment_water","PhotoID",
                             "Segmented", "FileType")

#list file names in each folder and add to date frame
for(i in 1:length(folder_names_list)) {
  folder_names <- folder_names_list
  folder_path <- paste0(my_path_photos, folder_names,"/Final/")
  file_names <- list.files(folder_path)
  file_path <- paste0(folder_path, file_names)
  file_names_df_1 <- data.frame(text = file_path) %>% 
    separate(text, sep = "/", 
             into = c("Photos_folder", "Date", "Stage", "Event")) %>% 
    separate(Event, sep = " ",
             into = c("SpeciesID","Treatment_temp","Treatment_water","Suffix")) %>% 
    separate(Suffix, sep = "_",
             into = c("PhotoID", "Suffix")) %>% 
    separate(Suffix, into = c("Segmented", "FileType")) %>% 
    separate(SpeciesID, sep = "(?<=[A-Za-z])(?=[0-9])", into = c("Species", "SpeciesID")) %>% 
    mutate(SpeciesID = paste(Species, SpeciesID, sep="")) %>% 
    mutate(FileType = toupper(FileType)) %>% 
    mutate(Date = parse_datetime(Date))
  file_names_df <- rbind(file_names_df, file_names_df_1)
}

#Add week column by date
dates <- read_csv("data_raw/meta/Phase1_Dates.csv")
dates <- dates %>% 
  filter(Variable == "Photos")
file_names_df <- merge(file_names_df, dates, by=c("Species","Date"))

#create condensed version to add to color data, we don't need all this other stuff
file_add <- file_names_df %>% 
  select(-c("Photos_folder", "Stage", "PhotoID", "Segmented", "FileType", "Chamber", "Variable")) %>%
  arrange(Date) %>% 
  arrange(SpeciesID)
#rearrange column order
file_add <- file_add[, c(6, 2, 1, 3, 4, 5)]



###############################################################################################
# 2. Create data frame of rgb pixel colors for each photo, save with file name information
###############################################################################################

#example
#3D plot of colors in my image
get_colors("Phase1_Photos/2021-08-26/Segmented/PIPO11 Ambient Drought DSC01486_segmented.jpg") %>% 
  plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")

#define black color
black <- c(25, 25, 25)

#3D plot of colors in my image, without black
get_colors("Phase1_Photos/2021-08-26/Segmented/PIPO11 Ambient Drought DSC01486_segmented.jpg",
           exclude_col = black, exclude_rad = 60) %>% 
  plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")



#for-loop to combine names with colors

#make empty files names df
tree_rgb <- data.frame(matrix(ncol = 12, nrow = 0))
colnames(tree_rgb) <- c("Week", "Date", "Species", "SpeciesID", "Treatment_temp", "Treatment_water",
                        "red", "green", "blue",
                        "col_hex", "col_freq", "col_share")

#file paths
#change date per iteration
my_path_photos <- "Phase1_Photos/"
folder_names_list <- list.files(my_path_photos)
my_path_final <- "Phase1_Photos/2021-08-26/Final/"
file_names_final <- list.files(my_path_final)
file_names <- paste0(my_path_final, file_names_final)

for(i in 1:length(file_names)) {
  pic_crop <- file_names[i]
  file_add_1 <- file_add[i,]
  tree <- data.frame(get_colors(pic_crop, exclude_col = black, exclude_rad = 60))
  rgb <- rotate_df(as.data.frame(col2rgb(tree$col_hex)))
  rownames(rgb) <- c(1:nrow(tree))
  tree_rgb_1 <- cbind(file_add_1, rgb, tree, row.names = NULL)
  tree_rgb <- rbind(tree_rgb, tree_rgb_1)
}


###############################################################################################
# 3. Summarize the data frame by color groups
###############################################################################################


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


#3D plot of colors in my image, comparing all colors with summarized colors
tree_rgb %>% plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")
tree_rgb_sum %>% plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")




###############################################################################################
# 4. Summarize the data frame by color groups
###############################################################################################


# First, get a df of background colors
################################################################################

#make empty files names df
background_rgb <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(background_rgb) <- c("red", "green", "blue", "col_hex", "col_freq", "col_share")

#define paths
my_path_background <- "Phase1_Photos_Background/"
file_names_list <- list.files(my_path_background)
file_names <- paste0(my_path_background, file_names_list)
file_names

#for loop of all background photos
for(i in 1:length(file_names)) {
  pic <- file_names[i]
  background <- data.frame(get_colors(pic))
  rgb <- rotate_df(as.data.frame(col2rgb(background$col_hex)))
  rownames(rgb) <- c(1:nrow(background))
  background_rgb_1 <- cbind(rgb, background, row.names = NULL)
  background_rgb <- rbind(background_rgb, background_rgb_1)
}

#see colors of background
background_rgb %>% plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")



# Second, filter to exclude grey colors
################################################################################

#save hex codes as vector colors_ignore
colors_ignore <- background_rgb$col_hex

#create new df without grey colors
tree_rgb_sum_filter <- subset(tree_rgb_sum,!(col_hex %in% colors_ignore))

#adjust col_total and col_share columns
tree_rgb_sum_filter <- tree_rgb_sum_filter %>% 
  group_by(Week, Date, Species, SpeciesID, Treatment_temp, Treatment_water) %>% 
  mutate(col_total = sum(col_freq)) %>% 
  mutate(col_share = round(100*(col_freq/col_total), digits = 2)) %>% 
  arrange(SpeciesID, Date, desc(col_share))



# Third, filter to exclude small number of pixels
################################################################################

#here we have some room to play around with different thresholds to best
#display the data. I'm choosing to remove colors with less than 100 pixels
#(which is about 0.01% of pixels per photo). Filtering this way removes about
#1/2 of the data points, but only about ~1% of the total data. Seems to be the 
#best choice to maximize data while reducing file size!

hist(tree_rgb_sum_filter$col_freq, breaks = 200) #before

#filter to exclude infrequent colors
tree_rgb_sum_filter <- tree_rgb_sum_filter %>%
  filter(col_freq >= 100) %>%
  arrange(desc(col_share))

#check out a histogram to see pixel distribution before and after
hist(Phase1_Photos$col_freq, breaks = 200) #after

#see colors of background
tree_rgb_sum_filter %>% plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")


#DONE with data frame!


###############################################################################################
# 5. Vizualization examples
###############################################################################################

#read_csv
Phase1_Data_Photos_Avg <- read_csv("data_analysis/Phase1_Data_Photos_Avg.csv")

# must arrange according to order of graph display
# First, all ambient displayed (week 1 - end). Then all hw (week 1-end)
# So, arrange by treatment_temp, then week. 
Phase1_Data_Photos_graph <- Phase1_Data_Photos_Avg %>% 
  filter(Treatment_water == "Drought", Treatment_temp == "Ambient") %>% 
  mutate(PercentRed = round(PercentRed, digits = 1)) %>% 
  mutate(label = paste0(PercentRed, " %"))

Phase1_Data_Photos_graph <- transform(Phase1_Data_Photos_graph,
                                      Species = factor(Species, levels = c("PIPO", "PIED", "PSME", "PIEN", "PIFL"))) %>% 
  arrange(Species, Treatment_temp, Week, green_only, desc(red_only))

levels(Phase1_Data_Photos_graph$Species)

#set colors
colors <- Phase1_Data_Photos_graph$col_hex

#viz dividing line between green and brown
Phase1_Data_Photos_graph %>% 
  ggplot(aes(x = red,
             y = green,
             color = colors)) +
  geom_point(color = colors) +
  geom_abline(intercept = 0,
              slope = 1,
              linetype = "dashed",
              size = 1.5) +
  theme_minimal()

#graph of color change over time
Phase1_Data_Photos_graph %>% 
  ggplot(aes(x = Week,
             y = col_share,
             fill = colors)) +
  geom_col(fill = colors) +
  facet_grid(Species ~ .) +
  geom_errorbar(aes(x = Week,
                    ymin = (PercentGreen - SD_PercentRed),
                    ymax = (PercentGreen + SD_PercentRed))) +
  scale_x_continuous(breaks = 1:36) +
  ylab("Percent Green") +
  xlab("Weeks") +
  labs(title = "Colors over Time") +
  theme_minimal()

#######################
# just one species
Phase1_Data_Photos_graph_PIPO <- Phase1_Data_Photos_Avg %>% 
  filter(Species == "PIPO", Treatment_water == "Drought", Treatment_temp == "Ambient") %>% 
  mutate(PercentRed = round(PercentRed, digits = 1)) %>% 
  mutate(label = paste0(PercentRed, " %")) %>% 
  arrange(Species, Treatment_temp, Week, green_only, desc(red_only))

#set colors
colors <- Phase1_Data_Photos_graph_PIPO$col_hex

#graph of color change over time
Phase1_Data_Photos_graph_PIPO %>% 
  ggplot(aes(x = Week,
             y = col_share,
             fill = colors)) +
  geom_col(fill = colors) +
  facet_grid(Species ~ .) +
  geom_errorbar(aes(x = Week,
                    ymin = (PercentGreen - SD_PercentRed),
                    ymax = (PercentGreen + SD_PercentRed))) +
  scale_x_continuous(breaks = 1:36) +
  ylab("Percent Green") +
  xlab("Weeks") +
  labs(title = "Colors over Time - ponderosa pine") +
  theme_minimal()
