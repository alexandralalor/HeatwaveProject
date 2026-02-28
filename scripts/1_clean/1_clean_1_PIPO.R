#Data wrangling script - PIPO
#Alexandra Lalor
#allielalor@arizona.edu
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2026-02-28

#working directory
setwd("~/Desktop/R Projects/HeatwaveProject")

#load tidyverse
library(tidyverse)

#read in data
Phase1_PIPO_Dead <- read_csv(file = "data_raw/plant_data/Phase1_PIPO_Dead.csv")
Phase1_PIPO_PercentBrown <- read_csv(file = "data_raw/plant_data/Phase1_PIPO_PercentBrown.csv")
Phase1_PIPO_Porometer <- read_csv(file = "data_raw/plant_data/Phase1_PIPO_Porometer.csv")
Phase1_PIPO_Weight <- read_csv(file = "data_raw/plant_data/Phase1_PIPO_Weight.csv")

Phase2_PIPO_Dead <- read_csv(file = "data_raw/plant_data/Phase2_PIPO_Dead.csv")
Phase2_PIPO_PercentBrown <- read_csv(file = "data_raw/plant_data/Phase2_PIPO_PercentBrown.csv")
Phase2_PIPO_Porometer <- read_csv(file = "data_raw/plant_data/Phase2_PIPO_Porometer.csv")
Phase2_PIPO_Weight <- read_csv(file = "data_raw/plant_data/Phase2_PIPO_Weight.csv")



#this PIPO data wrangling is based of of successful PIED wrangling
#for more in depth details of my process, start with data_wrangling_PIED



#my first goal is to structurally rearrange PIPO data
#(Dead, PercentBrown, Porometer, Weight)
#then I want to stitch all PIPO data together for 1 comprehensive PIPO dataframe


#my data has Xs embedded, and I need to change these to 0 or NA to make R happy
#right now I don't know which is better for analysis. I am proceeding with NA


###########WEIGHT

#Replace X with NA in all columns
Phase1_PIPO_Weight_NA <- Phase1_PIPO_Weight
Phase1_PIPO_Weight_NA[Phase1_PIPO_Weight_NA == "X"] <- NA

Phase2_PIPO_Weight_NA <- Phase2_PIPO_Weight
Phase2_PIPO_Weight_NA[Phase2_PIPO_Weight_NA == "X"] <- NA


#make sure that columns which previously had X are not characters, but integers instead
#make changes across all columns using "across()" function
#eventually I want a way for R to read "week_1.0:week_XXX" based on the last column
#rather than specifying which week to end on, bc every species is different
Phase1_PIPO_Weight_NA <- Phase1_PIPO_Weight_NA %>%
  mutate(across(starts_with("week"), as.integer))

Phase2_PIPO_Weight_NA <- Phase2_PIPO_Weight_NA %>%
  mutate(across(starts_with("week"), as.integer))

#view structure of data, should show all data as int
str(Phase1_PIPO_Weight_NA)
str(Phase2_PIPO_Weight_NA)


#I want to rearrange my data to have columns represent Week, Species, speciesID,and Variable
#pivot my data so that weeks become columns.
Phase1_PIPO_Weight_NA_pivot <- tidyr::pivot_longer(Phase1_PIPO_Weight_NA,
                                                   cols = starts_with("week"),
                                                   names_to = "Week",
                                                   values_to = "Weight_g",
                                                   names_prefix = "week_")
Phase2_PIPO_Weight_NA_pivot <- tidyr::pivot_longer(Phase2_PIPO_Weight_NA,
                                                   cols = starts_with("week"),
                                                   names_to = "Week",
                                                   values_to = "Weight_g",
                                                   names_prefix = "week_")

#Add phase as column
Phase1_PIPO_Weight_NA_pivot <- Phase1_PIPO_Weight_NA_pivot %>% 
  mutate(Phase = 1) %>% 
  relocate(Phase, .before = Species)
Phase2_PIPO_Weight_NA_pivot <- Phase2_PIPO_Weight_NA_pivot %>% 
  mutate(Phase = 2) %>% 
  relocate(Phase, .before = Species)
#Combine
PIPO_Weight_NA_pivot <- rbind(Phase1_PIPO_Weight_NA_pivot, Phase2_PIPO_Weight_NA_pivot)



###YAY it worked!! Now for all the other protocols! 


###########POROMETER

#Replace X with NA in all columns
Phase1_PIPO_Porometer_NA <- Phase1_PIPO_Porometer
Phase1_PIPO_Porometer_NA[Phase1_PIPO_Porometer_NA == "X"] <- NA

Phase2_PIPO_Porometer_NA <- Phase2_PIPO_Porometer
Phase2_PIPO_Porometer_NA[Phase2_PIPO_Porometer_NA == "X"] <- NA


####IMPORTANT we don't want integers here! because Porometer readings have decimals
#make sure that columns which previously had X are not characters, but "double" instead
#make changes across all columns using "across()" function
Phase1_PIPO_Porometer_NA <- Phase1_PIPO_Porometer_NA %>% 
  mutate(across(starts_with("week"), as.double))

Phase2_PIPO_Porometer_NA <- Phase2_PIPO_Porometer_NA %>% 
  mutate(across(starts_with("week"), as.double))

#view structure of data, make sure numbers are not characters
str(Phase1_PIPO_Porometer_NA)
str(Phase2_PIPO_Porometer_NA)


#I want to rearrange my data to have columns represent Week, Species, speciesID,and Variable
#pivot my data so that weeks become columns.
Phase1_PIPO_Porometer_NA_pivot <- tidyr::pivot_longer(Phase1_PIPO_Porometer_NA,
                                                      cols = starts_with("week"),
                                                      names_to = "Week",
                                                      values_to = "Porometer",
                                                      names_prefix = "week_")
Phase2_PIPO_Porometer_NA_pivot <- tidyr::pivot_longer(Phase2_PIPO_Porometer_NA,
                                                      cols = starts_with("week"),
                                                      names_to = "Week",
                                                      values_to = "Porometer",
                                                      names_prefix = "week_")

#Add phase as column
Phase1_PIPO_Porometer_NA_pivot <- Phase1_PIPO_Porometer_NA_pivot %>% 
  mutate(Porometer = round(Porometer, digits = 1)) %>% 
  mutate(Phase = 1) %>% 
  relocate(Phase, .before = Species)

Phase2_PIPO_Porometer_NA_pivot <- Phase2_PIPO_Porometer_NA_pivot %>% 
  mutate(Porometer = round(Porometer, digits = 1)) %>% 
  mutate(Phase = 2) %>% 
  relocate(Phase, .before = Species)

#Combine
PIPO_Porometer_NA_pivot <- rbind(Phase1_PIPO_Porometer_NA_pivot, Phase2_PIPO_Porometer_NA_pivot)




###########PERCENTBROWN

#Replace X with NA in all columns
Phase1_PIPO_PercentBrown_NA <- Phase1_PIPO_PercentBrown
Phase1_PIPO_PercentBrown_NA[Phase1_PIPO_PercentBrown_NA == "X"] <- NA

Phase2_PIPO_PercentBrown_NA <- Phase2_PIPO_PercentBrown
Phase2_PIPO_PercentBrown_NA[Phase2_PIPO_PercentBrown_NA == "X"] <- NA


####all percents are read in a characters.... Must change this
#make sure that columns which previously had X are not characters, but "integer" instead
#make changes across all columns using "across()" function
Phase1_PIPO_PercentBrown_NA <- Phase1_PIPO_PercentBrown_NA %>% 
  mutate(across(.cols=starts_with("week"),.fns=str_remove, pattern="%")) %>%
  mutate(across(starts_with("week"), as.integer))

Phase2_PIPO_PercentBrown_NA <- Phase2_PIPO_PercentBrown_NA %>% 
  mutate(across(.cols=starts_with("week"),.fns=str_remove, pattern="%")) %>%
  mutate(across(starts_with("week"), as.integer))

#view structure of data, make sure numbers are not characters
str(Phase1_PIPO_PercentBrown_NA)
str(Phase2_PIPO_PercentBrown_NA)


#I want to rearrange my data to have columns represent Week, Species, speciesID,and Variable
#pivot my data so that weeks become columns.
Phase1_PIPO_PercentBrown_NA_pivot <- tidyr::pivot_longer(Phase1_PIPO_PercentBrown_NA,
                                                         cols = starts_with("week"),
                                                         names_to = "Week",
                                                         values_to = "PercentBrown",
                                                         names_prefix = "week_")
Phase2_PIPO_PercentBrown_NA_pivot <- tidyr::pivot_longer(Phase2_PIPO_PercentBrown_NA,
                                                         cols = starts_with("week"),
                                                         names_to = "Week",
                                                         values_to = "PercentBrown",
                                                         names_prefix = "week_")


#Add phase as column
Phase1_PIPO_PercentBrown_NA_pivot <- Phase1_PIPO_PercentBrown_NA_pivot %>% 
  mutate(Phase = 1) %>% 
  relocate(Phase, .before = Species)

Phase2_PIPO_PercentBrown_NA_pivot <- Phase2_PIPO_PercentBrown_NA_pivot %>% 
  mutate(Phase = 2) %>% 
  relocate(Phase, .before = Species)

#Combine
PIPO_PercentBrown_NA_pivot <- rbind(Phase1_PIPO_PercentBrown_NA_pivot, Phase2_PIPO_PercentBrown_NA_pivot)


###########DEAD

#No Xs in Dead data, but make new df to manipulate
Phase1_PIPO_Dead_NA <- Phase1_PIPO_Dead
Phase2_PIPO_Dead_NA <- Phase2_PIPO_Dead


#view structure of data, should be characters
str(Phase1_PIPO_Dead_NA)
str(Phase2_PIPO_Dead_NA)


#I want to rearrange my data to have columns represent Week, Species, speciesID,and Variable
#pivot my data so that weeks become columns.
Phase1_PIPO_Dead_NA_pivot <- tidyr::pivot_longer(Phase1_PIPO_Dead_NA,
                                                 cols = starts_with("week"),
                                                 names_to = "Week",
                                                 values_to = "Dead",
                                                 names_prefix = "week_")
Phase2_PIPO_Dead_NA_pivot <- tidyr::pivot_longer(Phase2_PIPO_Dead_NA,
                                                 cols = starts_with("week"),
                                                 names_to = "Week",
                                                 values_to = "Dead",
                                                 names_prefix = "week_")


#Add phase as column
Phase1_PIPO_Dead_NA_pivot <- Phase1_PIPO_Dead_NA_pivot %>% 
  mutate(Phase = 1) %>% 
  relocate(Phase, .before = Species)

Phase2_PIPO_Dead_NA_pivot <- Phase2_PIPO_Dead_NA_pivot %>% 
  mutate(Phase = 2) %>% 
  relocate(Phase, .before = Species)

#Combine
PIPO_Dead_NA_pivot <- rbind(Phase1_PIPO_Dead_NA_pivot, Phase2_PIPO_Dead_NA_pivot)


################################################################################
#now how can I stitch together these data frames?
#PIPO_Dead_NA_pivot
#PIPO_PercentBrown_NA_pivot
#PIPO_Porometer_NA_pivot
#PIPO_Weight_NA_pivot


#start with Weight & Porometer
#make sure all data gets in, given that only some species have porometer readings
#verify and make sure all obs are retained for combined data. do this with all=true
PIPO_1 <- merge(
  PIPO_Weight_NA_pivot,
  PIPO_Porometer_NA_pivot,
  by=c("Phase","Species","SpeciesID","Week"), all=TRUE)

#next merge PercentBrown and Dead
#these have half-weeks recorded, make sure these show up
#verify and make sure all obs are retained for combined data. do this with all=true
PIPO_2 <- merge(
  PIPO_PercentBrown_NA_pivot,
  PIPO_Dead_NA_pivot,
  by=c("Phase","Species","SpeciesID","Week"))

#now merge all together
#YAY working!!
PIPO <- merge(
  PIPO_1,
  PIPO_2,
  by=c("Phase","Species","SpeciesID","Week"), all=TRUE)


#finally, make a CSV!
write.csv(PIPO, "data_raw/plant_data_2/PIPO.csv", quote = FALSE, row.names = FALSE)
