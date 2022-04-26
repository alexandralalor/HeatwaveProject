#removing labels from segmented image using countcolors package
#alternative would be to crop image, but I'd rather just have 1 code than crop 5000+ images
#Alexandra Lalor
#allielalor@email.arizona.edu
#First created: 2022-04-14
#Last updated: 2022-04-25

library(tidyverse)
library(countcolors)
library(colorfindr) #for 3D image and get_colors
library(imager) #for autocrop
library(magick) 
library(jpeg)
library(tools) #for file naming


################################################################################
                     
#testing crop sizes

pic <- "E:/Phase 1 Data/Phase 1 Photos/December 3 2021/Segmented/PIFL27 Ambient Watered DSC03511_segmented.jpg"

pic1 <- image_read(pic)
pic1_crop <- image_crop(pic1, geometry = "0x3000")
print(pic1_crop)

################################################################################



#crop photo...
pic_crop <- "November 5 2021/Crop/PIED44 Ambient Drought DSC00340_segmented_masked.png"
pic_crop_1 <- load.image(pic_crop)
pic_crop_2 <- image_read(pic_crop) #magick
pic3 <- image_convert(pic_crop_2, "jpeg") #magick
print(pic3)
image_info(pic3)

################################################################################
#WORKING
#crop photo using magick
my_path <- "C:/Users/allie/Desktop/UofA/HW project/heatwave_analysis/Phase 1 Photos/November 5 2021/Segmented/"
my_path_crop <- "C:/Users/allie/Desktop/UofA/HW project/heatwave_analysis/Phase 1 Photos/November 5 2021/Crop/"

file_names_old <- list.files(my_path)

file_names_old_2 <- file_path_sans_ext(file_names_old)

file_names_crop <- paste0(file_names_old_2,"_crop",".jpg")




for (i in 1:length(file_names_old)) {
  file_name <- file_names_old[i]
  file_names_new <- file_names_crop[i]
  pic <- paste0(my_path, file_name)
  pic1 <- image_read(pic)
  pic1_crop <- image_crop(pic1, geometry = "0x3550")
  my_path_final <- paste0(my_path_crop, file_names_new)
  image_write(pic1_crop, path = my_path_final)
}



#question of how to make the saved image have the same naming structure, but add _crop
my_path <- "C:/Users/allie/Desktop/UofA/HW project/heatwave_analysis/Phase 1 Photos/November 5 2021/Segmented"

file_names_old <- list.files(my_path)
file_names_old
file_names_old_2 <- file_path_sans_ext(file_names_old)
file_names_old_2

file_names_new <- paste0(file_names_old_2,"_crop",".jpg")
file_names_new

my_path_new <- "C:/Users/allie/Desktop/UofA/HW project/heatwave_analysis/Phase 1 Photos/November 5 2021/Crop/"
my_path_new <- paste0(my_path_new, file_names_new)
my_path_new


#trying to make the label a color so that autocrop recgnizes it
label_colors <- get_colors(label)
label_colors_rgb <- col2rgb(label_colors$col_hex)

autocrop(pic1, label_colors_rgb) %>% plot()

#this works well, except for label
autocrop(pic_mask_1) %>% plot()

?magick


#ignore black, orange, and blue (where c(R,G,B))
#kind of cuts out too much...
orange.lower.1 <- c(0.0, 0.0, 0.0)
orange.upper.1 <- c(0.5, 0.2, 0.5)
orange.lower.2 <- c(0.5, 0.0, 0.0)
orange.upper.2 <- c(1.0, 0.4, 0.5)
orange.lower <- append(orange.lower.1, orange.lower.2)
orange.upper <- append(orange.upper.1, orange.upper.2)
black.lower <- c(0,0,0)
black.upper <- c(0.4, 0.3, 0.3)
blue.lower <- c(0.0, 0.0, 0.2)
blue.upper <- c(0.2, 0.2, 0.5)


#merge colors to single argument
background.upper.orange <- append(black.upper, orange.upper)
background.upper.all <- append(background.upper.orange, blue.upper)

background.lower.orange <- append(black.lower, orange.lower)
background.lower.all <- append(background.lower.orange, blue.lower)


#load in segmented picture
#perhaps load in folder instead?
#need script that can work on all pictures in a folder
pic <- "November 5 2021/PIED44 Ambient Drought DSC00340_segmented.jpg"

#for an entire folder
#countcolors::countColorsInDirectory(folder, color.range="rectangular"


#ignore all background
background.ignore <- countcolors::countColors(pic, color.range="rectangular",
                                              upper = background.upper.all,
                                              lower = background.lower.all,
                                              target.color=c("black", "black", "black", "black"),
                                              plotting = TRUE,
                                              save.indicator = TRUE)
?countcolors

#plot in 2D using colorcounter
#colordistance::plotPixels(pic_mask, lower = NULL, upper = NULL, n = 5000)

#plot in 3D using colorfindr
pic_mask <- "November 5 2021/PSME47 Ambient Drought DSC04435_segmented_masked.png"
pic_mask %>%
  get_colors() %>% 
  plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")

  


