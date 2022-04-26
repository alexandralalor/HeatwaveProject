#cropping images, saving them to new folder
#Alexandra Lalor
#allielalor@email.arizona.edu
#First created: 2022-04-25
#Last updated: 2022-04-25

library(tidyverse)
library(tools) #for file naming
library(magick) #for cropping


#create objects for old and new file paths
#file path for external hard drive
#adjust file path for each date
my_path_seg <- "E:/Phase 1 Data/Phase 1 Photos/December 3 2021/Segmented/"
my_path_crop <- "E:/Phase 1 Data/Phase 1 Photos/December 3 2021/Crop/"

file_names_old_jpg <- list.files(my_path_seg)
file_names_old <- file_path_sans_ext(file_names_old_jpg)
file_names_crop <- paste0(file_names_old,"_crop",".jpg")


#first pass 0 x 3450
#for loop to crop photo, rename it, and save it to "Crop" folder
for (i in 1:length(file_names_old_jpg)) {
  file_names <- file_names_old_jpg[i]
  file_names_new <- file_names_crop[i]
  pic <- paste0(my_path_seg, file_names)
  pic1 <- image_read(pic)
  pic1_crop <- image_crop(pic1, geometry = "0x3550")
  my_path_final <- paste0(my_path_crop, file_names_new)
  image_write(pic1_crop, path = my_path_final)
}

###############################################################################
#second pass 0 x 3250
#third pass 0 x 3000
my_path_seg <- "E:/Phase 1 Data/Phase 1 Photos/December 3 2021/Crop/"
my_path_crop <- "E:/Phase 1 Data/Phase 1 Photos/December 3 2021/Crop2/"
file_names_old_jpg <- list.files(my_path_seg)

#for loop to refined already cropped photo
for (i in 1:length(file_names_old_jpg)) {
  file_names <- file_names_old_jpg[i]
  pic <- paste0(my_path_seg, file_names)
  pic1 <- image_read(pic)
  pic1_crop <- image_crop(pic1, geometry = "0x3000")
  my_path_final <- paste0(my_path_crop, file_names)
  image_write(pic1_crop, path = my_path_final)
}
