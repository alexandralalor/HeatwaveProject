#get_colors with colorfindr package, create a dataframe from masked image
#Alexandra Lalor
#allielalor@email.arizona.edu
#First created: 2022-04-14
#Last updated: 2022-04-19


#load libraries
library(tidyverse)
library(colorfindr)
library(sjmisc) #for rotate_df
library(ggtern) #for rbg2hex
#library(countcolors)

#load in pic_segmented_masked
pic_mask <- "Phase 1 Photos/November 5 2021/Practice/PSME47 Ambient Drought DSC04435_segmented_masked.png"

################################################################################
#save file name information,
#first separate by "/", then by " ", then by "_"
#better file naming could shorten this code...
file_name <- data.frame(text = pic_mask) %>% 
  separate(text, sep = "/", 
           into = c("Date","Event")) %>% 
  separate(Event, sep = " ",
           into = c("SpeciesID","Treatment_temp","Treatment_water","Suffix")) %>% 
  separate(Suffix, sep = "_",
           into = c("PhotoID","Segmented","Suffix")) %>% 
  separate(Suffix, into = c("Masked", "FileType")) %>% 
  mutate(FileType = tolower(FileType)) %>% 
  mutate(Date = parse_datetime(Date,
                               format = "%B %d %Y"))

#create condensed version to add to data, we don't need all of this
file_add <- file_name %>% 
  select(-c("Treatment_temp", "Treatment_water", "Segmented", "Masked", "PhotoID","FileType"))

################################################################################
#create a data frame from get_colors
#exclude black color
tree <- data.frame(get_colors(pic_mask, exclude_col = "black"))

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
  xlab("November")

#try to find a way to arrange the y-axis by most frequent colors to least frequent


