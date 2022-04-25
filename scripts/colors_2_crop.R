#cropping images, saving them to new folder
#Alexandra Lalor
#allielalor@email.arizona.edu
#First created: 2022-04-25
#Last updated: 2022-04-25

library(tidyverse)
#library(countcolors) #do I actually need this?
#library(colorfindr) #for 3D image and get_colors
library(tools) #for file naming
library(magick) #for cropping


#create objects for old and new file paths
#file path for local computer
my_path_seg <- "C:/Users/allie/Desktop/UofA/HW project/heatwave_analysis/Phase 1 Photos/November 5 2021/Segmented/"
my_path_crop <- "C:/Users/allie/Desktop/UofA/HW project/heatwave_analysis/Phase 1 Photos/November 5 2021/Crop/"

file_names_old_jpg <- list.files(my_path_seg)
file_names_old <- file_path_sans_ext(file_names_old_jpg)
file_names_crop <- paste0(file_names_old,"_crop",".jpg")

#file path for external hard drive
#adjust file path for each date
my_path_seg <- "E:/Phase 1 Data/Phase 1 Photos/August 26 2021/Segmented/"
my_path_crop <- "E:/Phase 1 Data/Phase 1 Photos/August 26 2021/Crop/"

file_names_old_jpg <- list.files(my_path_seg)
file_names_old <- file_path_sans_ext(file_names_old_jpg)
file_names_crop <- paste0(file_names_old,"_crop",".jpg")


#for loop to crop photo, rename it, and save it to "Crop" folder
for (i in 1:length(file_names_old_jpg)) {
  file_name <- file_names_old_jpg[i]
  file_names_new <- file_names_crop[i]
  pic <- paste0(my_path_seg, file_name)
  pic1 <- image_read(pic)
  pic1_crop <- image_crop(pic1, geometry = "0x3450")
  my_path_final <- paste0(my_path_crop, file_names_new)
  image_write(pic1_crop, path = my_path_final)
}
