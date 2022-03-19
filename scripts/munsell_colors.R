#using the "Munsell Plant Tissue Color Book 2012 revision"
#Allie Lalor
#allielalor@email.arizona.edu
#First created: 2022-03-01
#Last updated: 2022-03-17

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


install.packages("tidyverse")
library(tidyverse)
install.packages("colorfindr")
library(colorfindr)
update.packages(colorfindr)
install.packages("rlang")
remove.packages("rlang")

#install Munsell R Package
#install.packages("devtools")
library(devtools)
devtools::install_github("cwickham/munsell")
library(munsell)
#install.packages("testthat")
#install.packages("ggplot2")
library(testthat)
library(ggplot2)

#colors packagea
#install.packages("countcolors")
library(countcolors)
#install.packages("colorfindr")


#package to load images
#install.packages("readbitmap")
library(readbitmap)
#install.packages("imager")
#install.packages("magrittr")




#image1 <- load.image("November 5 2021/PIED01 Ambient Watered DSC02441.JPG")
#plot(image1)

#image1 <- jpeg::readJPEG("November 5 2021/PIED01 Ambient Watered DSC02441.JPG")
#colordistance::plotPixels(image1, lower=NULL, upper=NULL, n=5000)


#count colors
colordistance::plotPixels("November 5 2021/PIED01 Ambient Watered DSC02441.JPG", lower = NULL, upper = NULL, n = 5000)
colordistance::plotPixels("November 5 2021/PIEN05 Ambient+HW Watered DSC02608.JPG", lower = NULL, upper = NULL, n = 5000)

kmeans.clusters <- colordistance::getKMeanColors("November 5 2021/PIED01 Ambient Watered DSC02441.JPG", n=13, plotting = FALSE)
colordistance::extractClusters(kmeans.clusters)

image <- jpeg::readJPEG("November 5 2021/PIED01 Ambient Watered DSC02441.JPG")
image2 <- jpeg::readJPEG("November 5 2021/PIEN05 Ambient+HW Watered DSC02608.JPG")

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

three.colors <- countcolors::countColors("November 5 2021/PIEN05 Ambient+HW Watered DSC02608.JPG", 
                                         color.range="spherical", 
                                         center = c(orange.center, blue.center, black.center), 
                                         radius = c(0.2, 0.1, 0.06),
                                         bg.lower=NULL, bg.upper=NULL, plotting = TRUE,
                                         target.color=c("white", "white", "white"))



#munsell
plot_mnsl("N 1/0")




#colorfindr
#get colors

#trying to load in JPG images
#not working
#PIED01_pic <- system.file("November 5 2021/PIED01 Ambient Watered DSC02441.JPG", package="colorfindr")
#PIED01_pic2 <- load.image("November 5 2021/PIED01 Ambient Watered DSC02441.JPG")
#PIED01_pic3 <- jpeg::readJPEG("November 5 2021/PIED01 Ambient Watered DSC02441.JPG")
#PIED01_pic4 <- readJPEG("November 5 2021/PIED01 Ambient Watered DSC02441.JPG")


library("jpeg")
library("tiff")
library("magick")
library("dplyr")



get_colors("November 5 2021/PIED01 Ambient Watered DSC02441.JPG") %>% 
  plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")

?colorfindr


grey_colors <- gray.colors(200, start = 0, end = 1, gamma = 2.2, alpha = NULL, rev = FALSE)
blue_colors <- rgb(10, 17, 85, maxColorValue=255)
orange_colors




#a bunch of script for trying to read images in
#why can't I read images in?

#error message: Error in check_format(img):
#file does not appear to be a BMP, JPEG, PNG, TIFF, or SVG

#wait is this just it??
pic1 <- "November 5 2021/PIED01 Ambient Watered DSC02441.JPG"
get_colors(pic1)



#wait it think maybe I have the wrong file format? Is JPG the same as JPEG?
install.packages("rgdal")
library(rgdal)
library(jpeg)
library(tiff)


pic1 <- "November 5 2021/PIED01 Ambient Watered DSC02441.JPG"
get_colors(pic1)


       
#install.packages("BiocManager")
library(BiocManager)
BiocManager::install("EBImage")
library(EBImage)

image3 <- readImage("November 5 2021/PIED01 Ambient Watered DSC02441.JPG")

print(image3)
get_colors(image3)


install.packages('png')
library('png')
pngImage=readPNG("November 5 2021/TIFF/PIED01 Ambient Watered DSC02441.png")
get_colors(image)

#seems like magick package only works for svg (?) images
library(magick)

image <- image_read("November 5 2021/TIFF/PIED01 Ambient Watered DSC02441.png")
get_colors(image)

img8 <- readTIFF("November 5 2021/TIFF/PIED01 Ambient Watered DSC02441.tif")
img2 <- readGDAL("November 5 2021/PIED01 Ambient Watered DSC02441.JPG")
image <- readJPEG("November 5 2021/PIED01 Ambient Watered DSC02441.JPG")
img4 <- image_read("November 5 2021/TIFF/PIED01 Ambient Watered DSC02441.jpeg")
get_colors(img4)

#ok sooo I think this is working for reading images?
library(imager)
img3 <- load.image("November 5 2021/PIED01 Ambient Watered DSC02441.jpeg")
img7 <- load.image("November 5 2021/TIFF/PIED01 Ambient Watered DSC02441.tif")
#converted JPG to jpeg to see if that makes a difference
#if I have to convert all my images anyway, which file format is best? TIFF?
#darn ok so this didn't work... but I still think file formating is the problem
img5 <- load.image("November 5 2021/TIFF/PIED01 Ambient Watered DSC02441.jpeg")
get_colors(img8)
# try this
#nope
img6 <- readJPEG("November 5 2021/TIFF/PIED01 Ambient Watered DSC02441.jpeg")
get_colors(img6)





#That's enough image loading stuff
################################################################################
# grey colors: 68,189
# blue colors: 107,124


pic1 %>% 
  get_colors(exclude_col = grey_colors,
             exclude_rad = 20)


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


#109,908 rows
get_colors("November 5 2021/PIED01 Ambient Watered DSC02441.JPG")
