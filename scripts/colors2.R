#new color script
#Allie Lalor
#First created: 2022-03-17
#Last updated: 2022-03-17

install.packages("phenopix")
library(phenopix)
#time series package
library(xts)


#unsupervised image classification R
install.packages("raster")
install.packages(c('rgdal'),repos = "http://cran.case.edu", configure.args=c("--with-proj-include=/packages/PROJ/6.1.0/include","--with-proj-lib=/packages/PROJ/6.1.0/lib"))
install.packages("rgdal")
library(raster)
library(tiff)
library(rgdal)
install.packages("RStoolbox")
library(RStoolbox)

?raster

#PIED01 <- stack("November 5 2021/TIFF/PIED01 Ambient Watered DSC02441.tif")

#PIED01 <- readTIFF("November 5 2021/TIFF/PIED01 Ambient Watered DSC02441.tif")

#this is working pretty well...
PIED01 <- raster("November 5 2021/TIFF/PIED01 Ambient Watered DSC02441.tif")
PIED01

#unsuperClass("November 5 2021/TIFF/PIED01 Ambient Watered DSC02441.tif")

# Hartigan-Wong - not great
# Lloyd - definitely better
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

#extractVIs(img.path, roi.path, vi.path = NULL, roi.name = NULL, plot = TRUE, begin = NULL, spatial = FALSE, date.code, npixels=1, file.type='.jpg', bind=FALSE, shift.matrix=NULL, ncores='all', log.file=NULL)
DrawMULTIROI("November 5 2021/PIED01 Ambient Watered DSC02441.JPG")
?DrawMULTIROI
#might need to draw area (roi) around point of interest. Could set area to whole picture
#gives a df of pixel values
extractVIs("November 5 2021/PIED01 Ambient Watered DSC02441.JPG",
           vi.path = "output",
           plot = T,
           )

?extractVIs

plotSpatial(data, param, roi.data.path, image.path, probs=c(0.01, 0.99), ...)

#spatialGreen
spatialGreen(filtered.data, fit, threshold, ncores='all', log.file=NULL)

