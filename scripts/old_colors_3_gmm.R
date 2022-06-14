#background foreground colors - classification with Gaussian Mixture Model
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-03-29
#Last updated: 2022-03-29

#testing

#take image, transform 3D array to 2D matrix

#load packages
library(tidyverse)
library(jpeg)
library(mclust)
library(ggplot2)
#convert array to image
library(EBImage)#dont think I need this one
library(imager)
library(raster)

#read image in as an array
#create script to run through all image files
pic1 <- "November 5 2021/PIED01 Ambient Watered DSC02441.JPG"
img1 <- readJPEG(pic1)

#store original image dimensions
height <- dim(img1)[1]
width <- dim(img1)[2]
color <- dim(img1)[3]

#change array to a matrix
img1_matrix <- matrix(img1, prod(dim(img1)[1:2]), dim(img1)[3])

#identify classifications
#python 2 classification working much better
#I tried 2, 3, and 4 classifications... not working plus takes 30+ mins
img1_gmm <- Mclust(data = img1_matrix, G = 4)
#isolate classification column
img1_class <- img1_gmm$classification
#create matrix from column
img1_matrix_class <- matrix(img1_class, nrow = height, ncol = width)
#change matrix to array
img1_array_class <- array(c(img1_matrix_class), dim = c(height, width, color))



#mask image, changing background to solid color
img1_mask <- img1
img1_mask[img1_array_class == 2] = 0
img1_mask[img1_array_class == 1] = 0
img1_mask[img1_array_class == 4] = 0

plot(raster::as.raster(img1_mask))

