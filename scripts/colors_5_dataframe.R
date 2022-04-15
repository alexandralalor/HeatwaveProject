#get_colors with colorfindr package
#Alexandra Lalor
#allielalor@email.arizona.edu
#First created: 2022-04-14
#Last updated: 2022-04-14

library(tidyverse)
library(countcolors)
library(colorfindr)
?countcolors
#library(colorfindr)
library(sjmisc) #for rotate_df


#load in pic_segmented_masked
pic_mask <- "November 5 2021/PSME47 Ambient Drought DSC04435_segmented_masked.png"

#exclude black color: 67,785 rows = working!
#area = adding col_freq across all rows
#get_colors(pic_mask, exclude_col = "black")

#create a data frame from get_colors
PSME47 <- data.frame(get_colors(pic_mask, exclude_col = "black"))

#add RGB data into data frame
colorsRGB <- rotate_df(as.data.frame(col2rgb(PSME47$col_hex)))
rownames(colorsRGB) <- c(1:67795)
PSME47_RGB <- cbind(colorsRGB, PSME47)




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
