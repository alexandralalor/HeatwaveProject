#background foreground colors - classification with Gaussian Mixture Model
#Alexandra Lalor
#allielalor@email.arizona.edu
#First created: 2022-03-29
#Last updated: 2022-03-29

#testing

#take image, transform 3D array to 2D matrix

#load packages
library(tidyverse)
library(jpeg)
library(mclust)
#convert array to image
library(EBImage)
library(imager)

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
img1_gmm <- Mclust(data = img1_matrix, G = 2)
#isolate classification column
img1_class <- img1_gmm$classification
#create matrix from column
img1_matrix_class <- matrix(img1_class, nrow = height, ncol = width)
#change matrix to array
img1_array_class <- array(c(img1_matrix_class), dim = c(height, width, color))



#mask image, changing background to solid color
img1_mask <- img1
img1_mask <- img1_mask[img1_array_class == 2] = c(0,0,0)

plot(raster::as.raster(img1_mask))

################################################################################

img1_mask2 <- as.cimg(img1_mask, dim = c(height, width, color))

img1_mask_save <- cimg(img1_mask)
save.image(img1_mask_save, "output/img1_mask.jpg", quality = 0.7)

writeJPEG(img1_mask, target = raw(), quality = 0.7, color.space = "RGB")
writeJPEG((img1_mask, ""))

Image.fromarray(img1_array_class)

plot(img1_mask)

