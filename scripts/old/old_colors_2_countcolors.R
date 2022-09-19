#testing countcolors
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-03-23
#Last updated: 2022-04-14

################################################################################
library(countcolors)
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


################################################################################
#load in segmented image
#need script that can work on all pictures in a folder
pic <- "November 5 2021/PIEN01 Ambient+HW Watered DSC05080_segmented.jpg"
pic <- "November 5 2021/PSME47 Ambient Drought DSC04435_segmented.jpg"
pic_crop <- "November 5 2021/PIEN01 Ambient+HW Watered DSC05080_segmented_crop.jpg"


#plot on 2D using colorcounter
colordistance::plotPixels(pic, lower = NULL, upper = NULL, n = 5000)


#ignore black, orange, and blue (where c(R,G,B))
orange.lower.1 <- c(0.0, 0.0, 0.0)
orange.upper.1 <- c(1.0, 0.2, 0.5)
orange.lower.2 <- c(0.5, 0.2, 0.0)
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

#rectangular ranges, test each color
blue.ignore <- countcolors::countColors(pic, color.range = "rectangular",
                                        upper = blue.upper, lower = blue.lower,
                                        target.color = "black",
                                        plotting = TRUE)
orange.ignore <- countcolors::countColors(pic, color.range="rectangular",
                                          upper = orange.upper, lower = orange.lower,
                                          target.color = "black",
                                          plotting = TRUE)
black.ignore <- countcolors::countColors(pic, color.range="rectangular",
                                         upper = black.upper, lower = black.lower,
                                         target.color = "black",
                                         plotting = TRUE)




