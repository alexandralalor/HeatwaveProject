#Data Viz Final Project
#Create data frame of rgb pixel colors for each photo, save with file name information
#allielalor@email.arizona.edu
#First created: 2022-05-02
#Last updated: 2022-06-08

#load libraries
library(tidyverse)
library(colorfindr) #for get_colors
library(sjmisc) #for rotate_df
library(ggtern) #for rbg2hex
library(countcolors) #for masking and reducing black colors to one point
library(tools) #for file naming

################################################################################
#file naming data frames

names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_August_26_2021.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_September_02_2021.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_September_09_2021.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_September_16_2021.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_September_24_2021.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_September_30_2021.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_October_07_2021.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_October_15_2021.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_October_21_2021.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_October_29_2021.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_November_05_2021.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_November_11_2021.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_November_19_2021.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_November_26_2021.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_December_03_2021.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_December_10_2021.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_December_17_2021.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_December_30_2021.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_January_06_2022.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_January_13_2022.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_January_21_2022.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_January_28_2022.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_February_04_2022.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_February_11_2022.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_February_18_2022.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_February_24_2022.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_March_03_2022.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_March_11_2022.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_March_18_2022.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_March_24_2022.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_March_31_2022.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_April_07_2022.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_April_15_2022.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_April_21_2022.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_April_29_2022.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_May_06_2022.csv")
# names_df <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_May_12_2022.csv")


#names_df_all <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_all.csv")


#create condensed version to add to color data, we don't need all this other stuff
file_add <- names_df %>% 
  select(-c("Drive", "Data_folder", "Phase1_folder", "Photos_folder", "Stage", "PhotoID", "Segmented", "FileType", "Chamber", "Variable")) %>%
  arrange(Date) %>% 
  arrange(SpeciesID)
#rearrange column order
file_add <- file_add[, c(6, 2, 1, 3, 4, 5)]


#define black color
black <- c(25, 25, 25)

#make empty files names df
tree_rgb <- data.frame(matrix(ncol = 12, nrow = 0))
colnames(tree_rgb) <- c("Week", "Date", "Species", "SpeciesID", "Treatment_temp", "Treatment_water",
                        "red", "green", "blue",
                        "col_hex", "col_freq", "col_share")

########################################
## Change file path using date folder ##
########################################
# August 26 2021
# September 2 2021
# September 9 2021
# September 16 2021
# September 24 2021
# September 30 2021
# October 7 2021
# October 15 2021
# October 21 2021
# October 29 2021
# November 5 2021
# November 11 2021
# November 19 2021
# November 26 2021
# December 3 2021
# December 10 2021
# December 17 2021
# December 30 2021
# January 6 2022
# January 13 2022
# January 21 2022
# January 28 2022
# February 4 2022
# February 11 2022
# February 18 2022
# February 24 2022
# March 3 2022
# March 11 2022
# March 18 2022
# March 24 2022
# March 31 2022
# April 7 2022
# April 15 2022
# April 21 2022
# April 29 2022
# May 6 2022
# May 12 2022
######################################    

#file paths
#change date per iteration
my_path_photos <- "E:/Data/Phase1_Data/Phase1_Photos/"
folder_names_list <- list.files(my_path_photos)
my_path_final <- "E:/Data/Phase1_Data/Phase1_Photos/August 26 2021/Final/"
file_names_final <- list.files(my_path_final)
file_names <- paste0(my_path_final, file_names_final)

for(i in 1:length(file_names)) {
  pic_crop <- file_names[i]
  file_add_1 <- file_add[i,]
  tree <- data.frame(get_colors(pic_crop, exclude_col = black, exclude_rad = 60))
  rgb <- rotate_df(as.data.frame(col2rgb(tree$col_hex)))
  rownames(rgb) <- c(1:nrow(tree))
  tree_rgb_1 <- cbind(file_add_1, rgb, tree, row.names = NULL)
  tree_rgb <- rbind(tree_rgb, tree_rgb_1)
}

#check that all plants are added
tree_rgb %>% 
  summarize(species = unique(Species))

tree_rgb %>% 
  group_by(Date) %>% 
  summarize(week = unique(Week))


#Now go to rgb_sum script to summarize the data
#alternatively you can save this data using the code below, although they are very large files

#save files
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_August_26_2021.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_September_02_2021.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_September_09_2021.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_September_16_2021.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_September_24_2021.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_September_30_2021.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_October_07_2021.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_October_15_2021.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_October_21_2021.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_October_29_2021.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_November_05_2021.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_November_11_2021.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_November_19_2021.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_November_26_2021.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_December_03_2021.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_December_10_2021.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_December_17_2021.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_December_30_2021.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_January_06_2022.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_January_13_2022.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_January_21_2022.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_January_28_2022.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_February_04_2022.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_February_11_2022.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_February_18_2022.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_February_24_2022.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_March_03_2022.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_March_11_2022.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_March_18_2022.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_March_24_2022.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_March_31_2022.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_April_07_2022.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_April_15_2022.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_April_21_2022.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_April_29_2022.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_May_06_2022.csv", quote=FALSE, row.names = FALSE)
# write.csv(tree_rgb, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_May_12_2022.csv", quote=FALSE, row.names = FALSE)


#read in files
# tree_rgb_August_26_2021 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_August_26_2021.csv")
# tree_rgb_September_02_2021 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_September_02_2021.csv")
# tree_rgb_September_09_2021 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_September_09_2021.csv")
# tree_rgb_September_16_2021 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_September_16_2021.csv")
# tree_rgb_September_24_2021 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_September_24_2021.csv")
# tree_rgb_September_30_2021 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_September_30_2021.csv")
# tree_rgb_October_07_2021 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_October_07_2021.csv")
# tree_rgb_October_15_2021 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_October_15_2021.csv")
# tree_rgb_October_21_2021 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_October_21_2021.csv")
# tree_rgb_October_29_2021 <-read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_October_29_2021.csv")
# tree_rgb_November_05_2021 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_November_05_2021.csv")
# tree_rgb_November_11_2021 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_November_11_2021.csv")
# tree_rgb_November_19_2021 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_November_19_2021.csv")
# tree_rgb_November_26_2021 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_November_26_2021.csv")
# tree_rgb_December_03_2021 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_December_03_2021.csv")
# tree_rgb_December_10_2021 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_December_10_2021.csv")
# tree_rgb_December_17_2021 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_December_17_2021.csv")
# tree_rgb_December_30_2021 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_December_30_2021.csv")
# tree_rgb_January_06_2022 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_January_06_2022.csv")
# tree_rgb_January_13_2022 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_January_13_2022.csv")
# tree_rgb_January_21_2022 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_January_21_2022.csv")
# tree_rgb_January_28_2022 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_January_28_2022.csv")
# tree_rgb_February_04_2022 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_February_04_2022.csv")
# tree_rgb_February_11_2022 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_February_11_2022.csv")
# tree_rgb_February_18_2022 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_February_18_2022.csv")
# tree_rgb_February_24_2022 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_February_24_2022.csv")
# tree_rgb_March_03_2022 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_March_03_2022.csv")
# tree_rgb_March_11_2022 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_March_11_2022.csv")
# tree_rgb_March_18_2022 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_March_18_2022.csv")
# tree_rgb_March_24_2022 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_March_24_2022.csv")
# tree_rgb_March_31_2022 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_March_31_2022.csv")
# tree_rgb_April_07_2022 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_April_07_2022.csv")
# tree_rgb_April_15_2022 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_April_15_2022.csv")
# tree_rgb_April_21_2022 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_April_21_2022.csv")
# tree_rgb_April_29_2022 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_April_29_2022.csv")
# tree_rgb_May_06_2022 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_May_06_2022.csv")
# tree_rgb_May_12_2022 <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_May_12_2022.csv")


#combine all dates
# tree_rgb_all <- rbind(tree_rgb_August_26_2021, 
#                       tree_rgb_September_02_2021, tree_rgb_September_09_2021, tree_rgb_September_16_2021, tree_rgb_September_24_2021, tree_rgb_September_30_2021,
#                       tree_rgb_October_07_2021, tree_rgb_October_15_2021, tree_rgb_October_21_2021, tree_rgb_October_29_2021,
#                       tree_rgb_November_05_2021, tree_rgb_November_11_2021, tree_rgb_November_19_2021, tree_rgb_November_26_2021,
#                       tree_rgb_December_03_2021, tree_rgb_December_10_2021, tree_rgb_December_17_2021, tree_rgb_December_30_2021,
#                       tree_rgb_January_06_2022, tree_rgb_January_13_2022, tree_rgb_January_21_2022, tree_rgb_January_28_2022,
#                       tree_rgb_February_04_2022, tree_rgb_February_11_2022, tree_rgb_February_18_2022, tree_rgb_February_24_2022,
#                       tree_rgb_March_03_2022, tree_rgb_March_11_2022, tree_rgb_March_18_2022, tree_rgb_March_24_2022, tree_rgb_March_31_2022,
#                       tree_rgb_April_07_2022, tree_rgb_April_15_2022, tree_rgb_April_21_2022, tree_rgb_April_29_2022,
#                       tree_rgb_May_06_2022, tree_rgb_May_12_2022)


# write.csv(tree_rgb_all, "E:/Data/Phase1_Data/Phase1_Photos_Data/tree_rgb/tree_rgb_all.csv", quote=FALSE, row.names = FALSE)




#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# # # This is my attempt to loop all the dates rather than go one at a time

#file paths
#change date per iteration
# my_path_photos <- "data_raw/final_project/Photos/"
# folder_names_list <- list.files(my_path_photos)
# my_path_final <- "data_raw/final_project/Photos/September 9 2021/Final/"
# file_names_final <- list.files(my_path_final)
# file_names <- paste0(my_path_final, file_names_final)
# #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# 
# #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# my_path_photos <- "data_raw/final_project/Photos/"
# folder_names_list <- list.files(my_path_photos)
# 


# file_add %>% 
#   filter(Date == )
# 
# i=1
# file_path
# pic_crop <- file_path[i]
# pic_crop
# file_add_1 <- file_add[1,]
# file_add_1 <- matrix(ifelse((grepl("November 11 2021", pic_crop) == "TRUE"), file_add[(1),], file_add[(i+10),]))
# 
# file_add_1 <- ifelse((grepl("September 2 2021", pic_crop, fixed = T) == "TRUE"), names_df_August_26_2021, names_df_September_2_2021)
# file_add_1
# # 
# # i=11
# # #Make a df of picture colors, include file info for the specific tree
# # #Make sure that row in file_add is equal to number in file path...
# # for(i in 1:length(folder_names_list)) {
#   folder_names <- folder_names_list[i]
#   folder_path <- paste0(my_path_photos, folder_names,"/Final/")
#   file_names <- list.files(folder_path)
#   file_path <- paste0(folder_path, file_names)
# #   for(i in 1:length(file_path)) {
# #     pic_crop <- file_path[i]
# #     #file_add_1 <- ifelse(pic_crop file_add[i,]
# #     tree <- data.frame(get_colors(pic_crop, exclude_col = black, exclude_rad = 60))
# #     rgb <- rotate_df(as.data.frame(col2rgb(tree$col_hex)))
# #     rownames(rgb) <- c(1:nrow(tree))
# #     tree_rgb_1 <- cbind(file_add_1, rgb, tree, row.names = NULL)
# #     tree_rgb <- rbind(tree_rgb, tree_rgb_1)
# #   }
# # }  
#################################################################################



