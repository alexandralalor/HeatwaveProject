#getting filenames as columns practice
#Alexandra Lalor
#allielalor@email.arizona.edu
#First created: 2022-04-14
#Last updated: 2022-04-14

##############################################################################
#create vector with all file names
#never hard code file names, get them through list.files

experiment_files <- c(list.files(path = "November 5 2021/" , 
                                 full.names = TRUE))
experiment_files


#read all files
#keep file name in data using id = "filename"
#separate out file name into it's useful components using 
#separate(). Not useful components can be called X#
#delete not useful columns using select()

df <- matrix(nrow=1,ncol=8, byrow=F)
colnames(df) <- c("month","day","year","SpeciesID","Treatment_temp","Treatment_water","PhotoID","suffix")

?matrix

files_df <- as.data.frame(strsplit(experiment_files, split = " "))
files_df %>% 
  if 

files_df <- as.data.frame(strsplit(experiment_files, split = c("/")))
?strsplit

experiment_data <- read_tsv(experiment_files) %>% 
  separate(filename,into = c("month"," ","day"," ","year","/","SpeciesID"," ","Treatment_temp"," ","Treatment_water"," ","PhotoID","_","suffix"))

experiment_data <- read_tsv(experiment_files, id = "filename") %>% 
  separate(filename,
           c("month","day","year","SpeciesID","Treatment_temp","Treatment_water","PhotoID","suffix"),
           (sep = c(" ", "/", "_")))

filename
experiment_files



?separate
separate(experiment_files, sep = c(" ", "/", "_"))


#                             ,"Treatment_temp","Treatment_water","PhotoID")) %>% 
#  select(-X1, -X2, -X3, -X4, -X5, -X6, -X7) %>%
#  mutate(age = parse_number(age))
