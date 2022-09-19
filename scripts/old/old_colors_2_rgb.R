#Heatwave Project Phase 1
#Create data frame of rgb pixel colors for each photo, save with file name information
#This is my attempt to loop all the dates rather than go one at a time
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-06-18
#Last updated: 2022-06-18


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# # # This is my attempt to loop all the dates rather than go one at a time

# #file names
# names_df_all <- read_csv("E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_all.csv")


# #file paths
# #change date per iteration
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



