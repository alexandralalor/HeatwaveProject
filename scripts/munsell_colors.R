#using the "Munsell Plant Tissue Color Book 2012 revision"
#Allie Lalor
#allielalor@email.arizona.edu
#First created: 2022-03-01
#Last updated: 2022-03-16

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
library(colorfindr)

#package to load images
#install.packages("readbitmap")
library(readbitmap)
#install.packages("imager")
#install.packages("magrittr")
library(imager)



#image1 <- load.image("November 5 2021/PIED01 Ambient Watered DSC02441.JPG")
#plot(image1)

#image1 <- jpeg::readJPEG("November 5 2021/PIED01 Ambient Watered DSC02441.JPG")
#colordistance::plotPixels(image1, lower=NULL, upper=NULL, n=5000)


#count colors
colordistance::plotPixels("November 5 2021/PIED01 Ambient Watered DSC02441.JPG", lower = NULL, upper = NULL, n = 5000)

kmeans.clusters <- colordistance::getKMeanColors("November 5 2021/PIED01 Ambient Watered DSC02441.JPG", n=13, plotting = FALSE)
colordistance::extractClusters(kmeans.clusters)

image <- jpeg::readJPEG("November 5 2021/PIED01 Ambient Watered DSC02441.JPG")

#isolate grey color
#lower rectangle is toward black RBG 0,0,0
#upper rectangle is toward white RBG 1,1,1
#not really working
lower.rectangular <- c(0.25, 0.25, 0.25)
upper.rectangular <- c(.97, .97, .97)

lower.rectangular <- c(0,0,0)
upper.rectangular <- c(1,1,1)

image.rectangular <- countcolors::rectangularRange(image, 
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
orange.spherical <- countcolors::sphericalRange(image, 
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

three.colors <- countcolors::countColors("November 5 2021/PIED01 Ambient Watered DSC02441.JPG", 
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

get_colors("November 5 2021/PIED01 Ambient Watered DSC02441.JPG", exclude_col = "black")

get_colors("November 5 2021/PIED01 Ambient Watered DSC02441.JPG")
