#cropping images, saving them to new folder
#Alexandra Lalor
#allielalor@email.arizona.edu
#First created: 2022-04-25
#Last updated: 2022-04-28

library(tidyverse)
library(tools) #for file naming
library(magick) #for cropping
library(countcolors)


################################################################################

#testing crop sizes
#adjust crop dimensions until it looks good, then use this in the for loop

pic_test <- "E:/Phase 1 Data/Phase 1 Photos/October 15 2021/Crop/PIPO24 Ambient Watered DSC01732_segmented.jpg"

pic1_test <- image_read(pic_test)
pic1_test_crop <- image_crop(pic1_test, geometry = "0x3350")
print(pic1_test_crop)



################################################################################

#create objects for old and new file paths
#file path for external hard drive
#adjust file path for each date
my_path_seg <- "E:/Phase 1 Data/Phase 1 Photos/October 15 2021/Segmented/"
my_path_crop <- "E:/Phase 1 Data/Phase 1 Photos/October 15 2021/Crop1/"

file_names_old_jpg <- list.files(my_path_seg)
file_names_old <- file_path_sans_ext(file_names_old_jpg)
file_names_crop <- paste0(file_names_old,"_crop",".jpg")

#for loop to crop photo, rename it, and save it to "Crop" folder
for (i in 1:length(file_names_old_jpg)) {
  file_names <- file_names_old_jpg[i]
  file_names_new <- file_names_crop[i]
  pic <- paste0(my_path_seg, file_names)
  pic1 <- image_read(pic)
  pic1_crop <- image_crop(pic1, geometry = "0x3515")
  my_path_final <- paste0(my_path_crop, file_names_new)
  image_write(pic1_crop, path = my_path_final)
}


###############################################################################

my_path_seg <- "E:/Phase 1 Data/Phase 1 Photos/October 15 2021/Crop1/"
my_path_crop <- "E:/Phase 1 Data/Phase 1 Photos/October 15 2021/Crop2/"
file_names_old_jpg <- list.files(my_path_seg)

#for loop to refined already cropped photo
for (i in 1:length(file_names_old_jpg)) {
  file_names <- file_names_old_jpg[i]
  pic <- paste0(my_path_seg, file_names)
  pic1 <- image_read(pic)
  pic1_crop <- image_crop(pic1, geometry = "0x3350")
  my_path_final <- paste0(my_path_crop, file_names)
  image_write(pic1_crop, path = my_path_final)
}


################################################################################
# Cropped pictures in Crop folder, then masked and put into Final folder
# Takes a super long time compared to get_colors

#color definition!

#set black upper and lower bounds
black.lower <- c(0,0,0)
black.upper <- c(0.2, 0.2, 0.2)

#naming!
#adjust file path for each date
my_path_photos <- "data_raw/final_project/Photos/"
folder_names_list <- list.files(my_path_photos)
my_path_final <- "data_raw/final_project/Photos/August 26 2021/Final/"
my_path_crop <- "data_raw/final_project/Photos/August 26 2021/Crop/"
file_names_crop_jpg <- list.files(my_path_crop)
file_names_crop <- file_path_sans_ext(file_names_crop_jpg)
file_names_final <- paste0(file_names_crop,"_mask",".jpg")


#for loop! go through every folder, and for every file in the folder, count colors and save to new folder

#list file names in each folder and add to date frame
for(i in 1:length(file_names_crop_jpg)) {
  file_name <- file_names_crop_jpg[i]
  file_name_new <- file_names_final[i]
  pic <- paste0(my_path_crop, file_name)
  file_path_final <- paste0(my_path_final, file_name_new)
  countcolors::countColors(pic, color.range="rectangular",
                           upper = black.upper,
                           lower = black.lower,
                           target.color=c("black"),
                           save.indicator = file_path_final)
}


################################################################################
#look at 3D graph again, make sure black color is reduced to single point
pic_mask <- "data_raw/final_project/Photos/August 26 2021/Final/PIPO10 Ambient+HW Drought DSC00273_segmented_crop_mask.png"
pic_mask %>%
  get_colors() %>% 
  plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")
