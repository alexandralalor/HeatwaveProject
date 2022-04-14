#removing labels from segmented image using countcolors package
#alternative would be to crop image, but I'd rather just have 1 code than crop 5000+ images
#Alexandra Lalor
#First created: 2022-04-14
#Last updated: 2022-04-14

library(tidyverse)
library(countcolors)
library(colorfindr) #for 3D image

#load in segmented picture
#perhaps load in folder instead?
#need script that can work on all pictures in a folder
pic <- "November 5 2021/PIEN01 Ambient+HW Watered DSC05080_segmented.jpg"
pic <- "November 5 2021/PSME47 Ambient Drought DSC04435_segmented.jpg"
pic_crop <- "November 5 2021/PIEN01 Ambient+HW Watered DSC05080_segmented_crop.jpg"


#ignore black, orange, and blue (where c(R,G,B))
orange.lower.1 <- c(0.0, 0.0, 0.0)
orange.upper.1 <- c(1.0, 0.2, 0.5)
orange.lower.2 <- c(0.5, 0.2, 0.0)
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

#ignore all background
background.ignore <- countcolors::countColors(pic, color.range="rectangular",
                                              upper = background.upper.all,
                                              lower = background.lower.all,
                                              target.color=c("black", "black", "black"),
                                              #plotting = TRUE,
                                              save.indicator = TRUE)

#plot in 2D using colorcounter
#colordistance::plotPixels(pic, lower = NULL, upper = NULL, n = 5000)

#plot in 3D using colorfindr
pic_mask <- "November 5 2021/PSME47 Ambient Drought DSC04435_segmented_masked.png"
pic_mask %>% 
  get_colors() %>% 
  plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")

  


#for an entire folder
#countcolors::countColorsInDirectory(folder, color.range="rectangular"

