#Data Viz Final Project
#Create data frame with important information related to photos, using photo file name
#allielalor@email.arizona.edu
#First created: 2022-05-02
#Last updated: 2022-05-05

#load libraries
library(tidyverse)

###############################################################################
#load in images and name them based on file path
my_path_photos <-"data_raw/final_project/Photos/"
folder_names_list <- list.files(my_path_photos)

#make empty files names data frame
file_names_df <- data.frame(matrix(ncol = 13, nrow = 0))
colnames(file_names_df) <- c("Data_raw", "Final_project", "Photos", "Date", "Stage", 
                             "SpeciesID", "SpeciesID", "Treatment_temp","Treatment_water","PhotoID",
                             "Segmented", "Cropped", "FileType")

#list file names in each folder and add to date frame
for(i in 1:length(folder_names_list)) {
  folder_names <- folder_names_list[i]
  folder_path <- paste0(my_path_photos, folder_names,"/Final/")
  file_names <- list.files(folder_path)
  file_path <- paste0(folder_path, file_names)
  file_names_df_1 <- data.frame(text = file_path) %>% 
    separate(text, sep = "/", 
             into = c("Data_raw", "Final_project", "Photos", "Date", "Stage", "Event")) %>% 
    separate(Event, sep = " ",
             into = c("SpeciesID","Treatment_temp","Treatment_water","Suffix")) %>% 
    separate(Suffix, sep = "_",
             into = c("PhotoID","Segmented", "Suffix")) %>% 
    separate(Suffix, into = c("Cropped", "FileType")) %>% 
    separate(SpeciesID, sep = "(?<=[A-Za-z])(?=[0-9])", into = c("Species", "SpeciesID")) %>% 
    mutate(SpeciesID = paste(Species, SpeciesID, sep="")) %>% 
    mutate(FileType = toupper(FileType)) %>% 
    mutate(Date = parse_datetime(Date,
                                 format = "%B %d %Y"))
  file_names_df <- rbind(file_names_df, file_names_df_1)
}

file_names_df %>% 
  summarize(date = unique(Date))



#save all
write.csv(file_names_df, "data_raw/final_project/file_names/names_df_all.csv", quote=FALSE, row.names = FALSE)


#you can also split up the df to keep things organized by date...
names_df_August_26_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-08-26"))
write.csv(names_df_August_26_2021, "data_raw/final_project/file_names/names_df_August_26_2021.csv", quote=FALSE, row.names = FALSE)

names_df_November_11_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-11-11"))
write.csv(names_df_November_11_2021, "data_raw/final_project/file_names/names_df_November_11_2021.csv", quote=FALSE, row.names = FALSE)

names_df_November_19_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-11-19"))
write.csv(names_df_November_19_2021, "data_raw/final_project/file_names/names_df_November_19_2021.csv", quote=FALSE, row.names = FALSE)

names_df_November_5_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-11-05"))
write.csv(names_df_November_5_2021, "data_raw/final_project/file_names/names_df_November_5_2021.csv", quote=FALSE, row.names = FALSE)

names_df_October_15_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-10-15"))
write.csv(names_df_October_15_2021, "data_raw/final_project/file_names/names_df_October_15_2021.csv", quote=FALSE, row.names = FALSE)

names_df_October_21_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-10-21"))
write.csv(names_df_October_21_2021, "data_raw/final_project/file_names/names_df_October_21_2021.csv", quote=FALSE, row.names = FALSE)

names_df_October_29_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-10-29"))
write.csv(names_df_October_29_2021, "data_raw/final_project/file_names/names_df_October_29_2021.csv", quote=FALSE, row.names = FALSE)

names_df_October_7_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-10-07"))
write.csv(names_df_October_7_2021, "data_raw/final_project/file_names/names_df_October_7_2021.csv", quote=FALSE, row.names = FALSE)

names_df_September_16_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-09-16"))
write.csv(names_df_September_16_2021, "data_raw/final_project/file_names/names_df_September_16_2021.csv", quote=FALSE, row.names = FALSE)

names_df_September_2_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-09-02"))
write.csv(names_df_September_2_2021, "data_raw/final_project/file_names/names_df_September_2_2021.csv", quote=FALSE, row.names = FALSE)

names_df_September_24_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-09-24"))
write.csv(names_df_September_24_2021, "data_raw/final_project/file_names/names_df_September_24_2021.csv", quote=FALSE, row.names = FALSE)

names_df_September_30_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-09-30"))
write.csv(names_df_September_30_2021, "data_raw/final_project/file_names/names_df_September_30_2021.csv", quote=FALSE, row.names = FALSE)

names_df_September_9_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-09-09"))
write.csv(names_df_September_9_2021, "data_raw/final_project/file_names/names_df_September_9_2021.csv", quote=FALSE, row.names = FALSE)

