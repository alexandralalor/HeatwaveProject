#get_colors with colorfindr package, create a dataframe from masked image
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-04-14
#Last updated: 2022-05-02


#load libraries
library(tidyverse)
library(colorfindr) #for get_colors
library(sjmisc) #for rotate_df
library(ggtern) #for rbg2hex
#library(countcolors)

#load in pic_segmented_crop
pic_crop <- "E:/Phase 1 Data/Phase 1 Photos/August 26 2021/Final/PIPO30 Ambient+HW Drought DSC00297_segmented_crop.jpg"

################################################################################
#save file name information,
#first separate by "/", then by " ", then by "_"
#better file naming could shorten this code...
file_name <- data.frame(text = pic_crop) %>% 
  separate(text, sep = "/", 
           into = c("Drive", "Phase", "Photos", "Date", "Stage", "Event")) %>% 
  separate(Event, sep = " ",
           into = c("SpeciesID","Treatment_temp","Treatment_water","Suffix")) %>% 
  separate(Suffix, sep = "_",
           into = c("PhotoID","Segmented","Suffix")) %>% 
  separate(Suffix, into = c("Cropped", "FileType")) %>% 
  mutate(FileType = tolower(FileType)) %>% 
  mutate(Date = parse_datetime(Date,
                               format = "%B %d %Y"))

#create condensed version to add to data, we don't need all of this
file_add <- file_name %>% 
  select(-c("Drive", "Phase", "Photos", "Stage", "Treatment_temp", "Treatment_water", "Segmented", "Cropped", "PhotoID", "FileType"))

################################################################################
#create a data frame from get_colors
#exclude black color
tree <- data.frame(get_colors(pic_crop, exclude_col = "black"))

#add RGB data into data frame from the hex codes column
rgb <- rotate_df(as.data.frame(col2rgb(tree$col_hex)))
rownames(rgb) <- c(1:nrow(tree))
tree_rgb <- cbind(file_add, rgb, tree)

################################################################################
#reducing the data frame

#bining
# 8x8x8 = 526
# 8 blocks for each (rgb), each 32 pixels wide

#bin red
tree_rgb <- tree_rgb %>% 
  mutate(red_class = ifelse(red <= 32, "1-32", 
                            ifelse(red <= 64, "33-64", 
                                   ifelse(red <= 96, "65-96",
                                          ifelse(red <= 128, "97-128",
                                                 ifelse(red <= 160, "129-160",
                                                        ifelse(red <= 192, "161-192",
                                                               ifelse(red <= 224, "193-224", "225-256"))))))))
#bin green
tree_rgb <- tree_rgb %>% 
  mutate(green_class = ifelse(green <= 32, "1-32", 
                            ifelse(green <= 64, "33-64", 
                                   ifelse(green <= 96, "65-96",
                                          ifelse(green <= 128, "97-128",
                                                 ifelse(green <= 160, "129-160",
                                                        ifelse(green <= 192, "161-192",
                                                               ifelse(green <= 224, "193-224", "225-256"))))))))
#bin blue
tree_rgb <- tree_rgb %>% 
  mutate(blue_class = ifelse(blue <= 32, "1-32", 
                              ifelse(blue <= 64, "33-64", 
                                     ifelse(blue <= 96, "65-96",
                                            ifelse(blue <= 128, "97-128",
                                                   ifelse(blue <= 160, "129-160",
                                                          ifelse(blue <= 192, "161-192",
                                                                 ifelse(blue <= 224, "193-224", "225-256"))))))))
#summarize by bins
tree_rgb_sum <- tree_rgb %>% 
  group_by(red_class, green_class, blue_class) %>% 
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
tree_rgb_sum <- tree_rgb_sum[, c(1,2,3,4,5,6,8,7)]
#add file ID to summary df
tree_rgb_sum <- cbind(file_add, tree_rgb_sum)
#calculate total # pixels and percent of each color, add to summary df
tree_rgb_sum <- tree_rgb_sum %>% 
  mutate(col_total = sum(col_freq)) %>% 
  mutate(col_share = round(100*(col_freq/col_total), digits = 1))

################################################################################


#filter to exclude more grey and to most frequent colors
#grey colors: r=g=b
#here we can play around with thresholds for col_freq (# pixels in that bin)
tree_rgb_sum_filter <- tree_rgb_sum %>% 
  filter(!(red_class == green_class & red_class == blue_class)) %>% 
  filter(col_freq >= 1000) %>% 
  arrange(desc(col_share))

#method to ignore grey colors
#identify in color space where grey colors are, and isolate them
#add all these colors to tree_rgb_ignore dataframe
tree_rgb_ignore_1 <- tree_rgb_sum_all %>%
  group_by(Date, SpeciesID) %>%
  filter(red < 160) %>%
  filter((red_class == green_class & red_class == blue_class))

tree_rgb_ignore_2 <-tree_rgb_sum_all %>%
  group_by(Date, SpeciesID) %>%
  filter(red_class == "65-96" & green_class == "33-64" & blue_class == "33-64")

tree_rgb_ignore_3 <- tree_rgb_sum_all %>%
  group_by(Date, SpeciesID) %>%
  filter(red_class == "97-128" & green_class == "65-96" & blue_class == "33-64")

tree_rgb_ignore <- rbind(tree_rgb_ignore_1, tree_rgb_ignore_2, tree_rgb_ignore_3)


# tree_rgb_sum_August_26_2021 %>% 
#   if (tree_rgb_sum_August_26_2021$red_class %in% c("129-160","97-128","65-96","33-64","1-32")) {
#     filter(!(red_class == green_class & red_class == blue_class))
#   }
# 
#      
#      
# #  ifelse(red_class %in% c("129-160","97-128","65-96","33-64","1-32"), filter(!(red_class == green_class & red_class == blue_class)),{}) %>% 
#   plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")
# 
# 
#   filter(red < 150) %>% 
#   filter(filter(!(red_class == green_class & red_class == blue_class))) %>% 
#   plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")
# 
# ?ifelse
#   
# tree_rgb_sum_filter %>% 
#   group_by(Date, SpeciesID) %>% 
#   filter(col_share > 10) %>% 
#   make_palette()
# 
# tree_rgb_sum_filter <- tree_rgb_sum %>% 
#   filter(col_hex != "#707150") %>% 
#   filter(col_hex != "#909170") %>% 
#   filter(col_hex != "#4f5131") %>% 
#   filter(col_hex != "#505130") %>% 
#   filter(col_hex != "#707150") %>% 
#   filter(col_hex != "#8f9270") %>% 
#   filter(col_hex != "#505130") %>% 
#   filter(col_hex != "#707250") %>%
#   filter(col_hex != "#707151") %>%
#   filter(col_hex != "#8e9370") %>%
#   filter(col_hex != "#505230") %>% 
#   filter(col_hex != "#8e9270") %>%
#   filter(col_hex != "#4f5230") %>% 
#   filter(col_hex != "#8f9071")
# 
# 
# #grey colors: r=g=b
# #there seems to be a good threshold of green under 150 t0 180 which filters out
# #most grey, I just have to figure out how to make this into a list
# 
# tree_rgb_sum_filter <- tree_rgb_sum %>% 
#   group_by(Date, SpeciesID) %>% 
#   filter(green < 180) %>% 
#   filter(!(red_class == green_class & red_class == blue_class))
# 
# tree_rgb_sum_filter <- tree_rgb_sum_filter %>% 
#   group_by(Date, SpeciesID, Treatment_temp, Treatment_water) %>% 
#   mutate(col_total = sum(col_freq)) %>% 
#   mutate(col_share = round(100*(col_freq/col_total), digits = 1))
# 
# 
# tree_rgb_sum_filter <- tree_rgb_sum %>% 
#   filter(col_hex != "#525649") %>% 
#   filter(col_hex != "#323629") %>% 
#   filter(col_hex != "#555749") %>% 
#   filter(col_hex != "#545649") %>% 
#   filter(col_hex != "#565749") %>% 
#   filter(col_hex != "#333429") %>% 
#   filter(col_hex != "#555549") %>% 
#   filter(col_hex != "#323429") %>% 
#   filter(col_hex != "#343429") %>% 
#   filter(col_hex != "#333529") %>% 
#   filter(col_hex != "#747669") %>% 
#   filter(col_hex != "#727669") %>% 
#   filter(col_hex != "#737669") %>% 
#   filter(col_hex != "#747769") %>% 
#   filter(col_hex != "#757669") %>% 
#   filter(col_hex != "#545749") %>% 
#   filter(col_hex != "#333629") %>% 
#   filter(col_hex != "#929789") %>%
#   filter(col_hex != "#929689") %>%
#   filter(col_hex != "#343529") %>%
#   filter(col_hex != "#939689") %>%
#   filter(col_hex != "#949689") %>%
#   filter(col_hex != "#939789") %>%
#   filter(col_hex != "#949789") %>% 
#   filter(col_hex != "#505130") %>%
#   filter(col_hex != "#949789") %>% 
#   filter(col_hex != "#707150")


#remove some columns if you want
tree_rgb_sum_2 <- tree_rgb_sum %>% 
  select(-c(red_class, green_class, blue_class))

#colorfindr make_palette
tree_rgb_sum_filter %>% 
  make_palette(n = 3)

#visualize
colors <- tree_rgb_sum_filter$col_hex
as.double(tree_rgb_sum_filter$col_share)

tree_rgb_sum_filter %>% 
  ggplot(aes(x = SpeciesID,
             y = col_share,
             fill = col_hex)) +
  geom_col(fill = colors) +
  ylab("Color Percent") +
  xlab("August - PIPO30")

#try to find a way to arrange the y-axis by most frequent colors to least frequent


