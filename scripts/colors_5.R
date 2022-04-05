#trying out openCV, background imaging, and grabcut
#Alexandra Lalor
#allielalo@email.arizona.edu
#First created: 2022-04-04
#Last updated: 2022-04-04

install.packages("opencv")
library(opencv)
library(tidyverse)
library(ggplot2)
#images
library(jpeg)
library(imager)
library(raster)

#foreground background segmentation, task in computer vision, use opencv
#in theory all we are looking for is the background segmentation algorithms that come with it
#Thia task has many algorithms
#get a photo of the canvas under similar lighting
#use a probabilistic approach to removing non foreground pixels
#also use grabcut to mark a bounding space for plants