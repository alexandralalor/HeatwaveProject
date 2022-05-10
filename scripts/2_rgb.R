#Data Viz Final Project
#Create data frame of rgb pixel colors for each photo, save with file name information
#allielalor@email.arizona.edu
#First created: 2022-05-02
#Last updated: 2022-05-02

#load libraries
library(tidyverse)
library(colorfindr) #for get_colors
library(sjmisc) #for rotate_df
library(ggtern) #for rbg2hex
library(countcolors) #for masking and reducing black colors to one point
library(tools) #for file naming

################################################################################
#file naming data frames

#names_df_August_26_2021 <- read.csv("data_raw/final_project/file_names/names_df_August_26_2021.csv")
#names_df_November_11_2021 <- read.csv("data_raw/final_project/file_names/names_df_November_11_2021.csv")
#names_df_November_19_2021 <- read.csv("data_raw/final_project/file_names/names_df_November_19_2021.csv")
#names_df_November_5_2021 <- read.csv("data_raw/final_project/file_names/names_df_November_5_2021.csv")
#names_df_October_15_2021 <- read.csv("data_raw/final_project/file_names/names_df_October_15_2021.csv")
#names_df_October_21_2021 <- read.csv("data_raw/final_project/file_names/names_df_October_21_2021.csv")
#names_df_October_29_2021 <- read.csv("data_raw/final_project/file_names/names_df_October_29_2021.csv")
#names_df_October_7_2021 <- read.csv("data_raw/final_project/file_names/names_df_October_7_2021.csv")
#names_df_September_16_2021 <- read.csv("data_raw/final_project/file_names/names_df_September_16_2021.csv")
#names_df_September_2_2021 <- read.csv("data_raw/final_project/file_names/names_df_September_2_2021.csv")
#names_df_September_24_2021 <- read.csv("data_raw/final_project/file_names/names_df_September_24_2021.csv")
#names_df_September_30_2021 <- read.csv("data_raw/final_project/file_names/names_df_September_30_2021.csv")
#names_df_September_9_2021 <-read.csv("data_raw/final_project/file_names/names_df_September_9_2021.csv")

#names_df_all <- read.csv("data_raw/final_project/file_names/names_df_all.csv")


#create condensed version to add to color data, we don't need all this other stuff
file_add <- names_df_September_9_2021 %>% 
  select(-c("Data_raw", "Final_project", "Photos", "Stage", "PhotoID", "Segmented", "Cropped", "FileType")) %>%
  arrange(Date)

#define black color
black <- c(25, 25, 25)

#make empty files names df
tree_rgb <- data.frame(matrix(ncol = 11, nrow = 0))
colnames(tree_rgb) <- c("Date", "Species", "SpeciesID", "Treatment_temp", "Treatment_water",
                        "red", "green", "blue",
                        "col_hex", "col_freq", "col_share")

# "August 26 2021"
# "November 11 2021"
#"November 19 2021"
#"November 5 2021"
#"October 15 2021"
#"October 21 2021"
#"October 29 2021"
#"October 7 2021"
#"September 16 2021"
#"September 2 2021"
#"September 24 2021"
#"September 30 2021"
#"September 9 2021"


#file paths
#change date per iteration
my_path_photos <- "data_raw/final_project/Photos/"
folder_names_list <- list.files(my_path_photos)
my_path_final <- "data_raw/final_project/Photos/September 9 2021/Final/"
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
summarize(tree_rgb, species = unique(SpeciesID))

#save files
#write.csv(tree_rgb, "data_raw/final_project/tree_rgb/tree_rgb_August_26_2021.csv", quote=FALSE, row.names = FALSE)
#write.csv(tree_rgb, "data_raw/final_project/tree_rgb/tree_rgb_November_11_2021.csv", quote=FALSE, row.names = FALSE)
#write.csv(tree_rgb, "data_raw/final_project/tree_rgb/tree_rgb_November_19_2021.csv", quote=FALSE, row.names = FALSE)
#write.csv(tree_rgb, "data_raw/final_project/tree_rgb/tree_rgb_November_5_2021.csv", quote=FALSE, row.names = FALSE)
#write.csv(tree_rgb, "data_raw/final_project/tree_rgb/tree_rgb_October_15_2021.csv", quote=FALSE, row.names = FALSE)
#write.csv(tree_rgb, "data_raw/final_project/tree_rgb/tree_rgb_October_21_2021.csv", quote=FALSE, row.names = FALSE)
#write.csv(tree_rgb, "data_raw/final_project/tree_rgb/tree_rgb_October_29_2021.csv", quote=FALSE, row.names = FALSE)
#write.csv(tree_rgb, "data_raw/final_project/tree_rgb/tree_rgb_October_7_2021.csv", quote=FALSE, row.names = FALSE)
#write.csv(tree_rgb, "data_raw/final_project/tree_rgb/tree_rgb_September_16_2021.csv", quote=FALSE, row.names = FALSE)
#write.csv(tree_rgb, "data_raw/final_project/tree_rgb/tree_rgb_September_2_2021.csv", quote=FALSE, row.names = FALSE)
#write.csv(tree_rgb, "data_raw/final_project/tree_rgb/tree_rgb_September_24_2021.csv", quote=FALSE, row.names = FALSE)
#write.csv(tree_rgb, "data_raw/final_project/tree_rgb/tree_rgb_September_30_2021.csv", quote=FALSE, row.names = FALSE)
#write.csv(tree_rgb, "data_raw/final_project/tree_rgb/tree_rgb_September_9_2021.csv", quote=FALSE, row.names = FALSE)

#read in files
tree_rgb_August_26_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_August_26_2021.csv")
tree_rgb_November_11_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_November_11_2021.csv")
tree_rgb_November_19_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_November_19_2021.csv")
tree_rgb_November_5_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_November_5_2021.csv")
tree_rgb_October_15_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_October_15_2021.csv")
tree_rgb_October_21_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_October_21_2021.csv")
tree_rgb_October_29_2021 <-read.csv("data_raw/final_project/tree_rgb/tree_rgb_October_29_2021.csv")
tree_rgb_October_7_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_October_7_2021.csv")
tree_rgb_September_16_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_September_16_2021.csv")
tree_rgb_September_2_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_September_2_2021.csv")
tree_rgb_September_24_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_September_24_2021.csv")
tree_rgb_September_30_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_September_30_2021.csv")
tree_rgb_September_9_2021 <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_September_9_2021.csv")

#combine all dates, add weeks column
tree_rgb_all <- rbind(tree_rgb_August_26_2021, tree_rgb_November_11_2021, tree_rgb_November_19_2021,
                      tree_rgb_November_5_2021, tree_rgb_October_15_2021, tree_rgb_October_21_2021,
                      tree_rgb_October_29_2021, tree_rgb_October_7_2021, tree_rgb_September_16_2021,
                      tree_rgb_September_2_2021, tree_rgb_September_24_2021, tree_rgb_September_30_2021,
                      tree_rgb_September_9_2021)

tree_rgb_all <- tree_rgb_all %>% 
  mutate(Week = ifelse(Date == "2021-08-26", 1,
                       ifelse(Date == "2021-09-02", 2,
                              ifelse(Date == "2021-09-09", 3,
                                     ifelse(Date == "2021-09-16", 4,
                                            ifelse(Date == "2021-09-24", 5,
                                                   ifelse(Date == "2021-09-30", 6,
                                                          ifelse(Date == "2021-10-07", 7,
                                                                 ifelse(Date == "2021-10-15", 8,
                                                                        ifelse(Date == "2021-10-21", 9,
                                                                               ifelse(Date == "2021-10-29", 10,
                                                                                      ifelse(Date == "2021-11-05", 11,
                                                                                             ifelse(Date == "2021-11-11", 12, 13)))))))))))))
#reality check
tree_rgb_all %>% 
  group_by(Date) %>% 
  summarize(week = unique(Week))
summarize(tree_rgb_all, species = unique(SpeciesID))



write.csv(tree_rgb_all, "data_raw/final_project/tree_rgb/tree_rgb_all.csv", quote=FALSE, row.names = FALSE)


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



