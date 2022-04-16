#get_colors with colorfindr package
#Alexandra Lalor
#allielalor@email.arizona.edu
#First created: 2022-04-14
#Last updated: 2022-04-14


#load libraries
library(tidyverse)
library(colorfindr)
library(sjmisc) #for rotate_df
#library(countcolors)

#load in pic_segmented_masked
pic_mask <- "November 5 2021/PSME47 Ambient Drought DSC04435_segmented_masked.png"

#exclude black color: 67,785 rows = working!
#area = adding col_freq across all rows
#get_colors(pic_mask, exclude_col = "black")

#save file name information
file_name <- data.frame(text = pic_mask) %>% 
  separate(text, sep = "/", 
           into = c("Date","Event")) %>% 
  separate(Event, sep = " ",
           into = c("SpeciesID","Treatment_temp","Treatment_water","Suffix")) %>% 
  separate(Suffix, sep = "_",
           into = c("PhotoID","Segmented","Suffix")) %>% 
  separate(Suffix, into = c("Masked", "FileType")) %>% 
  mutate(FileType = toupper(FileType)) %>% 
  mutate(Date = parse_datetime(Date,
                               format = "%B %d %Y"))

file_add <- file_name %>% 
  select(-c("Treatment_temp", "Treatment_water", "Segmented", "Masked", "PhotoID","FileType"))


#create a data frame from get_colors
tree <- data.frame(get_colors(pic_mask, exclude_col = "black"))

#add RGB data into data frame
rgb <- rotate_df(as.data.frame(col2rgb(tree$col_hex)))
rownames(rgb) <- c(1:nrow(tree))
tree_rgb <- cbind(file_add, rgb, tree)

################################################################################
#reducing the data frame

#bining
16 x 16 x 16
32 x 32 x 32
8 x8 x8 = 526
8 blocks on each axis, each 32 pixels wide


#unsup class

pic_mask_1 <- raster("November 5 2021/PSME47 Ambient Drought DSC04435_segmented_masked.tiff")

pic_mask_2 <- unsuperClass(pic_mask_1, nSamples = 10000, nClasses = 50, nStarts = 25,
                         nIter = 100, norm = FALSE, clusterMap = TRUE,
                         algorithm = "Lloyd")

colors <- rainbow(50)
colors <- 
plot(pic_mask_2$map, col = colors, legend = FALSE, axes = FALSE, box = FALSE)
legend(1,1, legend = paste0("C",1:5), fill = colors,
       title = "Classes", horiz = TRUE,  bty = "n")










#munsell
load(file = "data_raw/sysdata.rda")

tree_rgb_munsell <- inner_join(tree_rgb, munsell.map, by = c("col_hex"="hex"))

tree_rgb %>% 
  group_by(col_hex) %>% 
  filter(col_hex == "#A79184")
  summarize(count = n())

hist(tree_rgb$col_freq, breaks = 50)

tree_rgb %>% 
  count(col_freq <= 5)

tree_rgb_2 <- tree_rgb %>% 
  filter(col_freq >= 25)

hist(tree_rgb_2$col_freq)

tree_rgb_3 <- tree_rgb_2 %>% 
  filter(col_freq >= 50)

hist(tree_rgb$col_freq)
hist(tree_rgb$red)
hist(tree_rgb$green)
hist(tree_rgb$blue)


################################################################################

#don't really understand this....
#run pca
prcomp_colors <- prcomp(colorsRGB,
                       rank = 2)

glimpse(prcomp_colors)

#give variance explained
data.frame(variable_name = c("Red", "Green", "Blue"),
           variance_explained = (prcomp_colors$sdev / sum(prcomp_colors$sdev)))

pca_colors_df <- prcomp_colors$x %>%
  data.frame()


pca_colors_df %>% 
  ggplot(aes(x = PC1)) +
  geom_boxplot()
