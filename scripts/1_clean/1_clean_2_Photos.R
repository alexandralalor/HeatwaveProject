#Heatwave Project Phase 1
#Data taken from tree_rgb_sum_all
#Moving from processing photos to cleaning up data and filtering grey colors
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-05-02
#Last updated: 2022-07-08

#load libraries
library(tidyverse)
library(colorfindr) #for get_colors
library(sjmisc) #for rotate_df

################################################################################
# First, get a df of background colors
################################################################################

#make empty files names df
background_rgb <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(background_rgb) <- c("red", "green", "blue", "col_hex", "col_freq", "col_share")

#define paths
my_path_background <- "Phase1_Photos_Background/"
file_names_list <- list.files(my_path_background)
file_names <- paste0(my_path_background, file_names_list)

#for loop of all background photos
for(i in 1:length(file_names)) {
  pic <- file_names[i]
  background <- data.frame(get_colors(pic))
  rgb <- rotate_df(as.data.frame(col2rgb(background$col_hex)))
  rownames(rgb) <- c(1:nrow(background))
  background_rgb_1 <- cbind(rgb, background, row.names = NULL)
  background_rgb <- rbind(background_rgb, background_rgb_1)
}

#save a csv
write.csv(background_rgb, "data_raw/photos/background_rgb.csv", quote=FALSE, row.names = FALSE)

################################################################################
# Second, filter to exclude grey colors
################################################################################

#read in Photos csvs
tree_rgb_sum_all <- read_csv("data_raw/photos/tree_rgb_sum_all.csv")
background_rgb <- read_csv("data_raw/photos/background_rgb.csv")

#isolate grey colors: grey is when R=G=B
tree_rgb_ignore <- tree_rgb_sum_all %>% 
  filter(red < 160) %>% 
  filter((red_class == green_class & red_class == blue_class))
tree_rgb_ignore <- tree_rgb_ignore %>% 
  select(c("red", "green", "blue", "col_hex", "col_freq", "col_share"))

#save all background colors as background_rgb
background_rgb <- rbind(background_rgb, tree_rgb_ignore)

#save hex codes as vector colors_ignore
colors_ignore <- background_rgb$col_hex

#create new df without grey colors
tree_rgb_sum_filter <- subset(tree_rgb_sum_all,!(col_hex %in% colors_ignore))


#finally, adjust col_total and col_share columns
tree_rgb_sum_filter <- tree_rgb_sum_filter %>% 
  group_by(Week, Date, Species, SpeciesID, Treatment_temp, Treatment_water) %>% 
  mutate(col_total = sum(col_freq)) %>% 
  mutate(col_share = round(100*(col_freq/col_total), digits = 2)) %>% 
  arrange(SpeciesID, Date, desc(col_share))

#use this space to test using 3d color plots
# tree_rgb_sum_filter %>% 
#   group_by(Date, SpeciesID) %>% 
#   plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")

#save csv
write.csv(tree_rgb_sum_filter, "data_clean/tree_rgb_sum_filter.csv", quote=FALSE, row.names = FALSE)

