#Heatwave Analysis / Data Viz Final Project
#Create data frame with important information related to photos, using photo file name
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-05-02
#Last updated: 2022-06-08

#load libraries
library(tidyverse)

###############################################################################
#load in images and name them based on file path
my_path_photos <- "E:/Data/Phase1_Data/Phase1_Photos/"
folder_names_list <- list.files(my_path_photos)

#make empty files names data frame
file_names_df <- data.frame(matrix(ncol = 13, nrow = 0))
colnames(file_names_df) <- c("Drive", "Data_folder", "Phase1_folder", "Photos_folder", "Date", "Stage", 
                             "SpeciesID", "SpeciesID", "Treatment_temp","Treatment_water","PhotoID",
                             "Segmented", "FileType")

#list file names in each folder and add to date frame
for(i in 1:length(folder_names_list)) {
  folder_names <- folder_names_list[i]
  folder_path <- paste0(my_path_photos, folder_names,"/Final/")
  file_names <- list.files(folder_path)
  file_path <- paste0(folder_path, file_names)
  file_names_df_1 <- data.frame(text = file_path) %>% 
    separate(text, sep = "/", 
             into = c("Drive", "Data_folder", "Phase1_folder", "Photos_folder", "Date", "Stage", "Event")) %>% 
    separate(Event, sep = " ",
             into = c("SpeciesID","Treatment_temp","Treatment_water","Suffix")) %>% 
    separate(Suffix, sep = "_",
             into = c("PhotoID", "Suffix")) %>% 
    separate(Suffix, into = c("Segmented", "FileType")) %>% 
    separate(SpeciesID, sep = "(?<=[A-Za-z])(?=[0-9])", into = c("Species", "SpeciesID")) %>% 
    mutate(SpeciesID = paste(Species, SpeciesID, sep="")) %>% 
    mutate(FileType = toupper(FileType)) %>% 
    mutate(Date = parse_datetime(Date,
                                 format = "%B %d %Y"))
  file_names_df <- rbind(file_names_df, file_names_df_1)
}

#check work
file_names_df %>% 
  summarize(date = unique(Date)) %>% 
  arrange(date)
file_names_df %>% 
  summarize(species = unique(Species))
file_names_df %>% 
  summarize(treatment_temp = unique(Treatment_temp))
file_names_df %>% 
  summarize(treatment_water = unique(Treatment_water))


#find errors
file_names_df %>% 
  filter(Treatment_water != "Watered", Treatment_water != "Drought") %>% 
  summarize(date = Date, SpeciesID = SpeciesID)
file_names_df %>% 
  filter(Segmented != "segmented") %>% 
  summarize(date = Date, SpeciesID = SpeciesID)
file_names_df %>% 
  filter(FileType != "JPG") %>% 
  summarize(date = Date, SpeciesID = SpeciesID)


#Add week column by date
dates <- read_csv("E:/Data/Phase1_Data/Cleaning/R CSVs/Phase1_All_Dates.csv")
dates <- dates %>% 
  filter(Variable == "Photos")
file_names_df <- merge(file_names_df, dates, by=c("Species","Date"))

#check differences
# anti_join(file_names_df, file_names_df_weeks) %>% 
#   summarize(date = Date, species = Species)


#save all
write.csv(file_names_df, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_all.csv", quote=FALSE, row.names = FALSE)


#you can also split up the df to keep things organized by date...
names_df_August_26_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-08-26"))
write.csv(names_df_August_26_2021, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_August_26_2021.csv", quote=FALSE, row.names = FALSE)

names_df_September_02_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-09-02"))
write.csv(names_df_September_02_2021, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_September_02_2021.csv", quote=FALSE, row.names = FALSE)

names_df_September_09_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-09-09"))
write.csv(names_df_September_09_2021, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_September_09_2021.csv", quote=FALSE, row.names = FALSE)

names_df_September_16_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-09-16"))
write.csv(names_df_September_16_2021, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_September_16_2021.csv", quote=FALSE, row.names = FALSE)

names_df_September_24_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-09-24"))
write.csv(names_df_September_24_2021, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_September_24_2021.csv", quote=FALSE, row.names = FALSE)

names_df_September_30_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-09-30"))
write.csv(names_df_September_30_2021, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_September_30_2021.csv", quote=FALSE, row.names = FALSE)

names_df_October_07_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-10-07"))
write.csv(names_df_October_07_2021, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_October_07_2021.csv", quote=FALSE, row.names = FALSE)

names_df_October_15_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-10-15"))
write.csv(names_df_October_15_2021, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_October_15_2021.csv", quote=FALSE, row.names = FALSE)

names_df_October_21_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-10-21"))
write.csv(names_df_October_21_2021, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_October_21_2021.csv", quote=FALSE, row.names = FALSE)

names_df_October_29_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-10-29"))
write.csv(names_df_October_29_2021, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_October_29_2021.csv", quote=FALSE, row.names = FALSE)

names_df_November_05_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-11-05"))
write.csv(names_df_November_05_2021, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_November_05_2021.csv", quote=FALSE, row.names = FALSE)

names_df_November_11_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-11-11"))
write.csv(names_df_November_11_2021, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_November_11_2021.csv", quote=FALSE, row.names = FALSE)

names_df_November_19_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-11-19"))
write.csv(names_df_November_19_2021, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_November_19_2021.csv", quote=FALSE, row.names = FALSE)

names_df_November_26_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-11-26"))
write.csv(names_df_November_26_2021, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_November_26_2021.csv", quote=FALSE, row.names = FALSE)

names_df_December_03_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-12-03"))
write.csv(names_df_December_03_2021, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_December_03_2021.csv", quote=FALSE, row.names = FALSE)

names_df_December_10_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-12-10"))
write.csv(names_df_December_10_2021, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_December_10_2021.csv", quote=FALSE, row.names = FALSE)

names_df_December_17_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-12-17"))
write.csv(names_df_December_17_2021, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_December_17_2021.csv", quote=FALSE, row.names = FALSE)

names_df_December_30_2021 <- file_names_df %>% 
  filter(Date == as.Date("2021-12-30"))
write.csv(names_df_December_30_2021, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_December_30_2021.csv", quote=FALSE, row.names = FALSE)

names_df_January_06_2022 <- file_names_df %>% 
  filter(Date == as.Date("2022-01-06"))
write.csv(names_df_January_06_2022, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_January_06_2022.csv", quote=FALSE, row.names = FALSE)

names_df_January_13_2022 <- file_names_df %>% 
  filter(Date == as.Date("2022-01-13"))
write.csv(names_df_January_13_2022, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_January_13_2022.csv", quote=FALSE, row.names = FALSE)

names_df_January_21_2022 <- file_names_df %>% 
  filter(Date == as.Date("2022-01-21"))
write.csv(names_df_January_21_2022, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_January_21_2022.csv", quote=FALSE, row.names = FALSE)

names_df_January_28_2022 <- file_names_df %>% 
  filter(Date == as.Date("2022-01-28"))
write.csv(names_df_January_28_2022, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_January_28_2022.csv", quote=FALSE, row.names = FALSE)

names_df_February_04_2022 <- file_names_df %>% 
  filter(Date == as.Date("2022-02-04"))
write.csv(names_df_February_04_2022, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_February_04_2022.csv", quote=FALSE, row.names = FALSE)

names_df_February_11_2022 <- file_names_df %>% 
  filter(Date == as.Date("2022-02-11"))
write.csv(names_df_February_11_2022, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_February_11_2022.csv", quote=FALSE, row.names = FALSE)

names_df_February_18_2022 <- file_names_df %>% 
  filter(Date == as.Date("2022-02-18"))
write.csv(names_df_February_18_2022, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_February_18_2022.csv", quote=FALSE, row.names = FALSE)

names_df_February_24_2022 <- file_names_df %>% 
  filter(Date == as.Date("2022-02-24"))
write.csv(names_df_February_24_2022, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_February_24_2022.csv", quote=FALSE, row.names = FALSE)

names_df_March_03_2022 <- file_names_df %>% 
  filter(Date == as.Date("2022-03-03"))
write.csv(names_df_March_03_2022, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_March_03_2022.csv", quote=FALSE, row.names = FALSE)

names_df_March_11_2022 <- file_names_df %>% 
  filter(Date == as.Date("2022-03-11"))
write.csv(names_df_March_11_2022, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_March_11_2022.csv", quote=FALSE, row.names = FALSE)

names_df_March_18_2022 <- file_names_df %>% 
  filter(Date == as.Date("2022-03-18"))
write.csv(names_df_March_18_2022, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_March_18_2022.csv", quote=FALSE, row.names = FALSE)

names_df_March_24_2022 <- file_names_df %>% 
  filter(Date == as.Date("2022-03-24"))
write.csv(names_df_March_24_2022, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_March_24_2022.csv", quote=FALSE, row.names = FALSE)

names_df_March_31_2022 <- file_names_df %>% 
  filter(Date == as.Date("2022-03-31"))
write.csv(names_df_March_31_2022, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_March_31_2022.csv", quote=FALSE, row.names = FALSE)

names_df_April_07_2022 <- file_names_df %>% 
  filter(Date == as.Date("2022-04-07"))
write.csv(names_df_April_07_2022, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_April_07_2022.csv", quote=FALSE, row.names = FALSE)

names_df_April_15_2022 <- file_names_df %>% 
  filter(Date == as.Date("2022-04-15"))
write.csv(names_df_April_15_2022, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_April_15_2022.csv", quote=FALSE, row.names = FALSE)

names_df_April_21_2022 <- file_names_df %>% 
  filter(Date == as.Date("2022-04-21"))
write.csv(names_df_April_21_2022, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_April_21_2022.csv", quote=FALSE, row.names = FALSE)

names_df_April_29_2022 <- file_names_df %>% 
  filter(Date == as.Date("2022-04-29"))
write.csv(names_df_April_29_2022, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_April_29_2022.csv", quote=FALSE, row.names = FALSE)

names_df_May_06_2022 <- file_names_df %>% 
  filter(Date == as.Date("2022-05-06"))
write.csv(names_df_May_06_2022, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_May_06_2022.csv", quote=FALSE, row.names = FALSE)

names_df_May_12_2022 <- file_names_df %>% 
  filter(Date == as.Date("2022-05-12"))
write.csv(names_df_May_12_2022, "E:/Data/Phase1_Data/Phase1_Photos_Data/file_names/names_df_May_12_2022.csv", quote=FALSE, row.names = FALSE)


















