#python practice with Devin Bayly

#pil image open

im = Image.open("PIED01 Ambient Watered.JPG")
import numpy as np
arr = np.array(im)

what is the r image library
Image.fromarray(arr)

filter_res = arr
arr [arr >80 and arr <120] = 0

################################################################################

pic1 <- "November 5 2021/PIED01 Ambient Watered DSC02441.JPG"
img1 <- readJPEG(pic1)

grey <- gray.colors(256, start = 0, end = 1, gamma = 2.2, alpha = NULL, rev = FALSE)

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


#isolate grey color using spheres
install.packages("plotwidgets")
install.packages("opencv")
library(opencv)
library(plotwidgets)




grey2 <- col2rgb(grey)

                
white.radii <- c(0.5, 0.3, 0.1)
grey.select <- countcolors::countColors(pic1,
                                        center=grey2,
                                        radius=white.radii,
                                        bg.lower=NULL,
                                        plotting=TRUE)


grey.select2 <- countcolors::sphericalRange(image, 
                                            center=white.ctr,
                                            radius=white.radii,
                                            color.pixels=FALSE,
                                            plotting=FALSE); names(grey.select2)



grey
?grey.colors




