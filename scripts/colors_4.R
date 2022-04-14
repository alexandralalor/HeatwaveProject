#colors_4
#Alexandra Lalor
#First created: 2022-04-14
#Last updated: 2022-04-14

library(tidyverse)
library(countcolors)

#load in picture
pic3 <- "November 5 2021/PIEN01 Ambient+HW Watered DSC05080_segmented_crop.jpg"

#plot on 2D using colorcounter
colordistance::plotPixels(pic3, lower = NULL, upper = NULL, n = 5000)


countcolors::countColors(pic3,
                         center=c(0.5,0.5,0.5),
                         bg.lower)

#perhaps we want to ignore black?
green.center <- c(0.24, 0.45, 0.24)
bg.upper <- c(0.2, 0.2, 0.45)
bg.lower <- c(0, 0, 0)

bg.ignore <- countcolors::countColors("norway.jpg", color.range="spherical", 
                                      center = green.center, radius = 0.15,
                                      bg.lower=bg.lower, bg.upper=bg.upper, plotting = TRUE)


#black center
black.ctr <- c(0,0,0)
black.radii <- c(0.2, 0.2, 0.2)
grey.select <- countcolors::countColors(image,
                                        center=white.ctr,
                                        radius=white.radii,
                                        bg.lower=NULL,
                                        plotting=TRUE)
