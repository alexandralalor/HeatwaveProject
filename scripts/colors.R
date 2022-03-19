#color script
#Allie Lalor
#allielalor@email.arizona.edu
#First created: 2022-03-17
#Last updated: 2022-03-19


#successful color package!
#colorfindr - get colors

#write a for-loop that can read in image, pull out # pixels per color
#save into data frame
#and then go next image and do same thing


#load packages
library(tidyverse)
library(colorfindr)
#for viewing image
library(countcolors)
library(imager)

im <- load.image(pic1)
plot(im)

#this is plotting all colors of my image
get_colors("November 5 2021/PIED01 Ambient Watered DSC02441.JPG") %>% 
  plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")


#let's make it easier by assining the image as an object first
#read in JPG image
#took me a surprisingly long time to figure this out
pic1 <- "November 5 2021/PIED01 Ambient Watered DSC02441.JPG"

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




#now we need to assign grey, blue, and orange colors
#is there a way I can save each color with it's own radius?
grey <- gray.colors(200, start = 0, end = 1, gamma = 2.2, alpha = NULL, rev = FALSE)
blue <- rgb(10, 17, 85, maxColorValue = 255)
orange <- rgb(220, 76, 15, maxColorValue = 255)
black

grey_blue <- append(grey, blue)
blue_orange <- append(blue, orange)
grey_blue_orange <- append(grey_blue, orange)

##################################
###found online
rgb(0, 1, 0)

rgb((0:15)/15, green = 0, blue = 0, names = paste("red", 0:15, sep = "."))

rgb(0, 0:12, 0, maxColorValue = 255) # integer input

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


################################################################################
#this is working but I want to refine the exclusion radius to be more specific

pic1 %>% 
  get_colors(exclude_col = grey_blue,
             exclude_rad = 30) %>% 
  plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")


pic1 %>% 
  get_colors(exclude_col = blue_orange,
             exclude_rad = 30) %>% 
  plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")

pic1 %>% 
  get_colors(exclude_col = grey_blue_orange,
             exclude_rad = 20) %>% 





#plotting in 3D

pic1 %>% 
  get_colors(exclude_col = grey,
             exclude_rad = 20) %>% 
  plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")


pic1 %>% 
  get_colors(exclude_col = blue,
             exclude_rad = 20) %>% 
  plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")


pic1 %>% 
  get_colors(exclude_col = orange,
             exclude_rad = 20) %>% 
  plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")



