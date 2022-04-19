#testing countcolors
#Alexandra Lalor
#allielalor@email.arizona.edu
#First created: 2022-03-23
#Last updated: 2022-03-23

#radius 0.1
orange1 <- c(0.80, 0.26, 0.04)
#radius 0.05
orange2 <- c(0.2, 0, 0)
#radius 0.1
orange3 <- c(0.5, 0.1, 0)
#radius 0.02
orange4 <- c(0.13, 0, 0)
#radius 0.02
orange5 <- c(0.4, 0.04, 0)
#radius 0.15
orange6 <- c(0.6, 0.2, 0.05)
#radius 0.1
orange7 <- c(0.84, 0.4, 0.13)

#radius 0.2
blue1 <- c(0.02, 0.06, 0.32)
#radius 0.1
blue2 <- c(0.1, 0.13, 0.33)
#radius 0.1
blue3 <- c(0.07, 0.06, 0.18)
#radius 0.1
blue4 <- c(0.02, 0, 0.13)
#radius 0.05
black <- c(0.1, 0, .05)



three.colors <- countcolors::countColors(pic1, 
                                         color.range="spherical", 
                                         center = c(orange1,orange2,orange3,orange4,orange5,orange6,orange7,
                                                    blue1, blue2, blue3, blue4,
                                                    black), 
                                         radius = c(0.1, 0.05, 0.1, 0.02, 0.02, 0.15, 0.1,
                                                    0.2, 0.1, 0.1, 0.1,
                                                    0.05),
                                         bg.lower=NULL, bg.upper=NULL, plotting = TRUE,
                                         target.color=c("white", "white", "white", "white", "white", "white", "white",
                                                        "white", "white", "white", "white", 
                                                        "white"))
