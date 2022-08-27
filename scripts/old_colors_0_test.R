#using the "Munsell Plant Tissue Color Book 2012 revision"
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-03-01
#Last updated: 2022-05-10

#uing existing R package of plant colors
#I'd like to use this color data to extract plant tissue color from my 
#experimental photos, and compute PercentBrown for each photo

#can I upload a photo, used this package to extract plant tissue pictures,
#and then calculate PercentBrown pixels?


#websites
#https://cran.r-project.org/web/packages/munsell/index.html
#https://github.com/cwickham/munsell/
#chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/viewer.html?pdfurl=https%3A%2F%2Fcran.r-project.org%2Fweb%2Fpackages%2Fmunsell%2Fmunsell.pdf&clen=98041&chunk=true


#find a code that can take an image and find
#hexidecimals in image (correspond to specific colors)

#potential R packages
#colorfindr
#countcolors

#potenitally need to turn image into raster for R to use

#munsell could take those hexidecimals and spit out color correlations

#write a for-loop that can read in image, pull out # pixels per color
#save into data frame
#and then go next image and do same thing




###############################################################################
###############################################################################
#package installations, potential packages I can use
library(tidyverse)
library(colorfindr)
update.packages(colorfindr)
install.packages("rlang")
remove.packages("rlang")
library(devtools)
library(munsell)
library(testthat)
library(ggplot2)
library(countcolors)
library(readbitmap)
library(phenopix)
#time series package
library(xts)

#loading images specifically
library(jpeg)
library(tiff)
library(magick)
library(dplyr)
library(BiocManager)
library(EBImage)
library(png)
library(rgdal)
library(jpeg)
library(tiff)
#seems like magick package only works for svg (?) images
library(magick)
library(imager)



################################################################################
################################################################################
#literally just trying to load in JPG images
#a bunch of script for trying to read images in
#why can't I read images in?
#wait it think maybe I have the wrong file format? Is JPG the same as JPEG?
#converted JPG to jpeg to see if that makes a difference
#if I have to convert all my images anyway, which file format is best? TIFF?
#darn ok so this didn't work... but I still think file formating is the problem
#not working

#this is what I'm using
img1 <- readJPEG("November 5 2021/PIED01 Ambient Watered DSC02441.JPG")
#other practice
img2 <- readJPEG("November 5 2021/TIFF/PIED01 Ambient Watered DSC02441.jpeg")
img3 <- readTIFF("November 5 2021/TIFF/PIED01 Ambient Watered DSC02441.tif")
img4 <- readGDAL("November 5 2021/PIED01 Ambient Watered DSC02441.JPG")
img5 <- image_read("November 5 2021/TIFF/PIED01 Ambient Watered DSC02441.jpeg")
img6 <- image_read("November 5 2021/TIFF/PIED01 Ambient Watered DSC02441.png")
img7 <- load.image("November 5 2021/PIED44 Ambient Drought DSC00340_segmented.jpg")
img8 <- load.image("November 5 2021/TIFF/PIED01 Ambient Watered DSC02441.tif")
img9 <- load.image("November 5 2021/TIFF/PIED01 Ambient Watered DSC02441.jpeg")
img10 <- readImage("November 5 2021/PIED01 Ambient Watered DSC02441.JPG")
img11 <- readPNG("November 5 2021/TIFF/PIED01 Ambient Watered DSC02441.png")


#test if working for colorfindr specifically
get_colors(img6)
#error message: Error in check_format(img):
#file does not appear to be a BMP, JPEG, PNG, TIFF, or SVG


#wait is this just it??
#YES
pic1 <- "November 5 2021/PIED01 Ambient Watered DSC02441.JPG"
get_colors(pic1)



###############################################################################
###############################################################################
#Ideas from 03-17-2022 working with Charlie
#unsupervised image classification
#phenopix package - spatial green, draw ROI...

#unsupervised image classification R
library(raster)
library(tiff)
library(rgdal)
library(RStoolbox)

#this is working pretty well...
PIED01 <- raster("November 5 2021/TIFF/PIED01 Ambient Watered DSC02441.tif")

# Hartigan-Wong - not great
# Lloyd - definitely better, let's use this
# MacQueen - not as good as lloyd it seems

PIED01_1 <- unsuperClass(PIED01, nSamples = 10000, nClasses = 2, nStarts = 25,
                         nIter = 100, norm = FALSE, clusterMap = TRUE,
                         algorithm = "Lloyd")

colors <- rainbow(2)
plot(PIED01_1$map, col = colors, legend = FALSE, axes = FALSE, box = FALSE)
legend(1,1, legend = paste0("C",1:5), fill = colors,
       title = "Classes", horiz = TRUE,  bty = "n")


###############################
image <- jpeg::readJPEG("November 5 2021/PIED01 Ambient Watered DSC02441.JPG")

DrawMULTIROI("November 5 2021/PIED01 Ambient Watered DSC02441.JPG")
#might need to draw area (roi) around point of interest. Could set area to whole picture
#gives a df of pixel values
extractVIs("November 5 2021/PIED01 Ambient Watered DSC02441.JPG",
           vi.path = "output",
           plot = T)

plotSpatial(data, param, roi.data.path, image.path, probs=c(0.01, 0.99), ...)

#spatialGreen
spatialGreen(filtered.data, fit, threshold, ncores='all', log.file=NULL)




################################################################################
################################################################################
#count colors package
#colordistance plot pixels is nice - plots pixels on 2d plane


colordistance::plotPixels("November 5 2021/PIED01 Ambient Watered DSC02441.JPG", 
                          lower = NULL, upper = NULL, n = 5000)
colordistance::plotPixels("November 5 2021/PIEN05 Ambient+HW Watered DSC02608.JPG", 
                          lower = NULL, upper = NULL, n = 5000)

kmeans.clusters <- colordistance::getKMeanColors("November 5 2021/PIED01 Ambient Watered DSC02441.JPG", 
                                                 n=13, plotting = FALSE)
colordistance::extractClusters(kmeans.clusters)


#isolate grey color
#lower rectangle is toward black RBG 0,0,0
#upper rectangle is toward white RBG 1,1,1
#not really working
lower.rectangular <- c(0.25, 0.25, 0.25)
upper.rectangular <- c(.97, .97, .97)

lower.rectangular <- c(0,0,0)
upper.rectangular <- c(1,1,1)

image.rectangular <- countcolors::rectangularRange(image2, 
                                               upper=upper.rectangular,
                                               lower=lower.rectangular,
                                               target.color = "white")


#isolate grey color using spheres
white.ctr <- c(0.9, 0.9)
white.radii <- c(0.5, 0.3, 0.1)
grey.select <- countcolors::countColors(image,
                                        center=white.ctr,
                                        radius=white.radii,
                                        bg.lower=NULL,
                                        plotting=TRUE)
grey.select2 <- countcolors::sphericalRange(image, 
                                               center=white.ctr,
                                               radius=white.radii,
                                               color.pixels=FALSE,
                                               plotting=FALSE); names(grey.select2)



#isolate orange color
#working
orange.center <- c(0.80, 0.26, 0.04)
orange1 <- c(0.2, 0, 0)
orange2 <- c(0.5, 0.1, 0)
orange.spherical <- countcolors::sphericalRange(image1, 
                                               center=orange.center,
                                               radius=0.2,
                                               color.pixels=FALSE,
                                               plotting=FALSE); names(orange.spherical)

image.spherical$img.fraction

countcolors::changePixelColor(image, orange.spherical$pixel.idx, target.color = "white")


#isolate blue color
#working
blue.center <- c(0.03, 0.05, 0.32)
blue.spherical <- countcolors::sphericalRange(image, 
                                               center=blue.center,
                                               radius=0.1,
                                               color.pixels=FALSE,
                                               plotting=FALSE); names(blue.spherical)

countcolors::changePixelColor(image, blue.spherical$pixel.idx, target.color = "white")

#isolate black color
#this is getting most black and some dark green. pretty much there.
#working
black.center <- c(0.1, 0, .05)
black.spherical <- countcolors::sphericalRange(image, 
                                               center=black.center,
                                               radius=0.06,
                                               color.pixels=FALSE,
                                               plotting=FALSE); names(black.spherical)

countcolors::changePixelColor(image, black.spherical$pixel.idx, target.color = "white")


#isolate orange, blue, and black colors

three.colors <- countcolors::countColors(pic1, 
                                         color.range="spherical", 
                                         center = c(orange.center, orange1, orange2, blue.center, black.center), 
                                         radius = c(0.2, 0.2, 0.2, 0.1, 0.06),
                                         bg.lower=NULL, bg.upper=NULL, plotting = TRUE,
                                         target.color=c("white", "white", "white", "white", "white"))



################################################################################
################################################################################
#munsell
#didn't get very far here...

plot_mnsl("N 1/0")
?munsell

load(file = "data_raw/sysdata.rda")

tree_rgb_munsell <- inner_join(tree_rgb, munsell.map, by = c("col_hex"="hex"))

tree_rgb %>% 
  group_by(col_hex) %>% 
  filter(col_hex == "#A79184")
summarize(count = n())

hist(tree_rgb$col_freq, breaks = 50)

tree_rgb %>% 
  count(col_freq <= 5)

tree_rgb_2 <- tree_rgb %>% 
  filter(col_freq >= 25)

hist(tree_rgb_2$col_freq)

tree_rgb_3 <- tree_rgb_2 %>% 
  filter(col_freq >= 50)

hist(tree_rgb$col_freq)
hist(tree_rgb$red)
hist(tree_rgb$green)
hist(tree_rgb$blue)



################################################################################
################################################################################
#colorfindr
#get colors

#109,908 rows
get_colors("November 5 2021/PIED01 Ambient Watered DSC02441.JPG")

#use this if I ever can read in a picture
pic1 %>% 
  get_colors(exclude_col = grey_colors,
             exclude_rad = 20)

#make my own colors
grey_colors <- gray.colors(200, start = 0, end = 1, gamma = 2.2, alpha = NULL, rev = FALSE)
blue_colors <- rgb(10, 17, 85, maxColorValue=255)
orange_colors


#AHHHHHHH YAYAYAYAYA

get_colors("November 5 2021/PIED01 Ambient Watered DSC02441.JPG",
           exclude_col = grey_colors,
           exclude_rad = 20) %>% 
  get_colors("November 5 2021/PIED01 Ambient Watered DSC02441.JPG",
           exclude_col = blue_colors,
           exclude_rad = 25)

get_colors("November 5 2021/PIED01 Ambient Watered DSC02441.JPG",
           exclude_col = grey_colors, 
           exclude_rad = 20) %>% 
  plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")
  
  
get_colors("November 5 2021/PIED01 Ambient Watered DSC02441.JPG",
           exclude_col = blue_colors,
           exclude_rad = 25) %>% 
  plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")


#I'm gonna play around with the exclude_rad for gray to see what's a good threshold
get_colors("November 5 2021/PIED01 Ambient Watered DSC02441.JPG", 
           exclude_col = gray_colors,
           exclude_rad = 10) %>% 
  plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")


get_colors("November 5 2021/PIED01 Ambient Watered DSC02441.JPG", 
           exclude_col = gray_colors,
           exclude_rad = 10)

################################################################################
#unsup class

pic_mask_1 <- raster("November 5 2021/PSME47 Ambient Drought DSC04435_segmented_masked.tiff")

pic_mask_2 <- unsuperClass(pic_mask_1, nSamples = 10000, nClasses = 50, nStarts = 25,
                           nIter = 100, norm = FALSE, clusterMap = TRUE,
                           algorithm = "Lloyd")

colors <- rainbow(50)
colors <- 
  plot(pic_mask_2$map, col = colors, legend = FALSE, axes = FALSE, box = FALSE)
legend(1,1, legend = paste0("C",1:5), fill = colors,
       title = "Classes", horiz = TRUE,  bty = "n")


################################################################################

#don't really understand this....
#run pca
prcomp_colors <- prcomp(colorsRGB,
                        rank = 2)

glimpse(prcomp_colors)

#give variance explained
data.frame(variable_name = c("Red", "Green", "Blue"),
           variance_explained = (prcomp_colors$sdev / sum(prcomp_colors$sdev)))

pca_colors_df <- prcomp_colors$x %>%
  data.frame()


pca_colors_df %>% 
  ggplot(aes(x = PC1)) +
  geom_boxplot()


################################################################################
#new background foreground method
library(tidyverse)
library(jpeg)
library(colorfindr) #for get_colors
library(sjmisc) #for rotate_df
library(ggtern) #for rbg2hex


img <- "C:/Users/allie/Desktop/UofA/HW project/heatwave_analysis/Photos/backdrop photos/DSC05348.JPG"
img1 <- readJPEG(img)

background <- get_colors(img)
rgb <- rotate_df(as.data.frame(col2rgb(background$col_hex)))
rownames(rgb) <- c(1:nrow(background))
backgound_rgb <- cbind(rgb, background)


background_hex <- background$col_hex

pic_crop <- "C:/Users/allie/Desktop/UofA/HW project/heatwave_analysis/Photos/backdrop photos/PIED01 Ambient Watered DSC00329.JPG"

tree <- get_colors(pic_crop)
tree_seg <- get_colors(pic_crop, exclude_col = background_hex)

tree_seg %>%
  plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")




#add RGB data into data frame from the hex codes column
rgb <- rotate_df(as.data.frame(col2rgb(tree$col_hex)))
rownames(rgb) <- c(1:nrow(tree))
tree_rgb <- cbind(file_add, rgb, tree)







