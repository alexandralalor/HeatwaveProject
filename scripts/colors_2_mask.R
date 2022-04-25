#removing labels from segmented image using countcolors package
#alternative would be to crop image, but I'd rather just have 1 code than crop 5000+ images
#Alexandra Lalor
#allielalor@email.arizona.edu
#First created: 2022-04-14
#Last updated: 2022-04-14

library(tidyverse)
library(countcolors)
library(colorfindr) #for 3D image and get_colors
library(imager) #for autocrop
library(magick) #
library(jpeg)

#crop photo
pic <- "November 5 2021/PIED44 Ambient Drought DSC00340_segmented.jpg"
pic1 <- load.image(pic)
label <- "November 5 2021/label.jpg"
label1 <- load.image(label)


pic_mask <- "November 5 2021/PIED44 Ambient Drought DSC00340_segmented_masked.png"
pic_mask_1 <- load.image(pic_mask) 
pic_mask_2 <- image_read(pic_mask) #magick
pic3 <- image_convert(pic_mask_2, "jpeg") #magick

print(pic2)
image_info(pic3)


#crop photo using magick
pic <- "November 5 2021/PIED44 Ambient Drought DSC00340_segmented.jpg"
pic1 <- image_read(pic)
pic1_crop <- image_crop(pic1, geometry = "0x3550")
print(pic1_crop)
#image_write(pic1_crop, path = "November 5 2021/crop/{file2}_crop.jpg")

#question of how to make the saved image have the same naming structure, but add _crop
file_name <- data.frame(text = pic) %>% 
  separate(text, sep = "/", 
           into = c("Date","Event")) %>% 
  separate(Event, sep = " ",
           into = c("SpeciesID","Treatment_temp","Treatment_water","Suffix")) %>% 
  separate(Suffix, sep = "_",
           into = c("PhotoID","Suffix")) %>% 
  separate(Suffix, into = c("Segmented", "FileType")) %>% 
  mutate(FileType = tolower(FileType)) %>% 
  mutate(Date = parse_datetime(Date,
                               format = "%B %d %Y"))

file <- file_name %>% 
  select(-c("Date","FileType","Segmented")) %>% 
  mutate(Crop = "crop")

file2 <- toString(file, sep = " ")
file2






#trying to make the label a color so that autocrop recgnizes it
label_colors <- get_colors(label)
label_colors_rgb <- col2rgb(label_colors$col_hex)

autocrop(pic1, label_colors_rgb) %>% plot()

#this works well, except for label
autocrop(pic_mask_1) %>% plot()

?magick


#ignore black, orange, and blue (where c(R,G,B))
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

  


