#getting filenames as columns
#Alexandra Lalor
#allielalor@email.arizona.edu
#First created: 2022-04-14
#Last updated: 2022-04-14

##############################################################################
#create data frame with all file names

experiment_files <- "November 5 2021/PSME47 Ambient Drought DSC04435_segmented_masked.png"

file <- data.frame(text = experiment_files) %>% 
  separate(text, sep = "/", 
           into = c("date","event")) %>% 
  separate(event, sep = " ",
           into = c("SpeciesID","Treatment_temp","Treatment_water","Suffix"))

#next I need to manipulate the date column to be consistent with my other data
file_dates <- file %>% 
  mutate(date = parse_datetime(date,
                                   format = "%B %d %Y"))

