#color script with colorfindr
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-03-17
#Last updated: 2022-03-19


#successful color package!
#colorfindr - get colors

#write a for-loop that can read in image, pull out # pixels per color
#save into data frame
#and then go next image and do same thing


#load packages
library(tidyverse)
library(colorfindr) #getcolors
#for viewing image
library(countcolors)
library(imager)
library(jpeg)

im <- load.image(pic1)
plot(im)

#this is plotting all colors of my image
get_colors("November 5 2021/PIED01 Ambient Watered DSC02441.JPG") %>% 
  plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")


#let's make it easier by assinging the image as an object first
#read in JPG image
#took me a surprisingly long time to figure this out
pic1 <- "November 5 2021/PIED01 Ambient Watered DSC02441.JPG"
pic2 <- "November 5 2021/PIEN01 Ambient+HW Watered DSC05080_segmented.jpg"
pic3 <- "November 5 2021/PIEN01 Ambient+HW Watered DSC05080_segmented_crop.jpg"
#check that image read in correctly
get_colors(pic1)

#how to look at image?
view(pic1)
plot(pic1)

col <- get_colors(pic1)
plot_colors(col)

countcolors::countColors(pic1,
                         center = grey_blue_orange)

#this works but do I have to re-save it as an object? Make sure imager pkg is loaded
im <- load.image(pic1)
plot(im)



grey
#now we need to assign grey, blue, and orange colors
#is there a way I can save each color with it's own radius?
#red blue green
grey <- gray.colors(256, start = 0, end = 1, gamma = 2.2, alpha = NULL, rev = FALSE)
blue <- rgb(10, 17, 85, maxColorValue = 255)
orange <- rgb(210, 87, 15, maxColorValue = 255)


black

grey_blue <- append(grey, blue)
blue_orange <- append(blue, orange)
grey_blue_orange <- append(grey_blue, orange)
grey_orange <- append(grey, orange)

##################################
###found online
rgb(0, 1, 0)

rgb((0:15)/15, green = 0, blue = 0, names = paste("red", 0:15, sep = "."))

rgb(0, 0:12, 0, maxColorValue = 255) # integer input
?rgb

ramp <- colorRamp(c("red", "white"))
rgb( ramp(seq(0, 1, length.out = 5)), maxColorValue = 255)
##################################

#to make sure it's working, the rows should be decreasing as we take out colors
get_colors()
#all colors: 109,908
#exclude grey colors: 68,189
#exclude blue colors: 107,124
#exclude orange colors: 106,846
#exclude grey and blue colors: 65,979
#exclude grey, blue, and orange colors

#continue to refine these, not 100% yet
pic1 %>% 
  get_colors(exclude_col = grey,
             exclude_rad = 20)

pic1 %>% 
  get_colors(exclude_col = blue,
             exclude_rad = 25)

pic1 %>% 
  get_colors(exclude_col = orange,
             exclude_rad = 20)




########################practice

pic1 <- "November 5 2021/PIED01 Ambient Watered DSC02441.JPG"
img1 <- readJPEG(pic1)
plot(raster::as.raster(img1))

#crop image
img1_crop <- img1[1501:3500, 2001:4500, ]
plot(raster::as.raster(img1_crop))
pic1_exclude <- pic1 %>% 
  get_colors(exclude_col = grey,
             exclude_rad = 20)




#plotting in 3D
pic %>% 
  get_colors(exclude_col = grey,
             exclude_rad =10) %>% 
  plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")


pic1 %>% 
  get_colors(exclude_col = blue,
             exclude_rad = 20) %>% 
  plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")


pic1 %>% 
  get_colors(exclude_col = grey_orange,
             exclude_rad = 10) %>% 
  plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")



#segmented
pic3 %>% 
  get_colors(exclude_col = "black",
             exclude_rad = 60) %>% 
  plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")

pic %>% 
  get_colors() %>% 
  plot_colors_3d(sample_size = 10000, marker_size = 2.5, color_space = "RGB")

background.ignore %>% 
  get_colors() %>% 
  plot_colors_3d(sample_size = 10000, marker_size = 2.5, color_space = "RGB")
