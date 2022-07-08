#Data wrangling script - Phase 1 PIED
#Alexandra Lalor
#allielalor@email.arizona.edu
#allielalor@gmail.com
#First created: 2022-02-01
#Last updated: 2022-03-15

#working directory
setwd("~/Desktop/UofA/HW project/analysis/HeatwaveProject")

#load tidyverse
library(tidyverse)

#read in data
Phase1_PIED_Dead <- read_csv(file = "data_raw/plant_data/Phase1_PIED_Dead.csv")
Phase1_PIED_PercentBrown <- read_csv(file = "data_raw/plant_data/Phase1_PIED_PercentBrown.csv")
Phase1_PIED_Porometer <- read_csv(file = "data_raw/plant_data/Phase1_PIED_Porometer.csv")
Phase1_PIED_Weight <- read_csv(file = "data_raw/plant_data/Phase1_PIED_Weight.csv")



#my first goal is to structurally rearrange PIED data
#(Dead, PercentBrown, Porometer, Weight)
#then I want to stitch all PIED data together for 1 comprehensive PIED dataframe


#my data has Xs embedded, and I need to change these to 0 or NA to make R happy
#right now I don't know which is better for analysis. So try both!
#start with weight data


###########PIED to 0
###########WEIGHT

#Replace X with 0 in all columns
Phase1_PIED_Weight_0 <- Phase1_PIED_Weight
Phase1_PIED_Weight_0[Phase1_PIED_Weight_0 == "X"] <- 0


#make sure that columns which previously had X are not characters, but integers instead
#make changes across all columns using "across()" function
#relative coding to keep weeks flexible
Phase1_PIED_Weight_0 <- Phase1_PIED_Weight_0 %>% 
  mutate(across(starts_with("week"), as.integer))

#view structure of data to make sure all data is int
str(Phase1_PIED_Weight_0)


#I want to rearrange my data to have columns represent Week, Species, speciesID,and Variable
#pivot my data so that weeks become columns.
Phase1_PIED_Weight_0_pivot <- tidyr::pivot_longer(Phase1_PIED_Weight_0,
                                                  cols = starts_with("week"),
                                                  names_to = "Week",
                                                  values_to = "Weight_g",
                                                  names_prefix = "week_")


###########PIED to NA
###########WEIGHT

#Replace X with NA in all columns
Phase1_PIED_Weight_NA <- Phase1_PIED_Weight
Phase1_PIED_Weight_NA[Phase1_PIED_Weight_NA == "X"] <- NA


#make sure that columns which previously had X are not characters, but integers instead
#make changes across all columns using "across()" function

#can't to the function below because Species and SpeciesID cannot be changed to NA
#Phase1_PIED_Weight_NA <- Phase1_PIED_Weight_NA %>%
#  mutate(across(where(is.character), as.integer))
Phase1_PIED_Weight_NA <- Phase1_PIED_Weight_NA %>% 
  mutate(across(starts_with("week"), as.integer))

#view structure of data, should show all data as int
str(Phase1_PIED_Weight_NA)


#I want to rearrange my data to have columns represent Week, Species, speciesID,and Variable
#pivot my data so that weeks become columns.
Phase1_PIED_Weight_NA_pivot <- tidyr::pivot_longer(Phase1_PIED_Weight_NA,
                                                   cols = starts_with("week"),
                                                   names_to = "Week",
                                                   values_to = "Weight_g",
                                                   names_prefix = "week_")


###YAY it worked!! Now for all the other protocols! 
#I'm going to keep working with NA data because I think it'll cause less problems in the future
#But 0 data is still above if I decide this makes more sense


###########POROMETER

#Replace X with NA in all columns
Phase1_PIED_Porometer_NA <- Phase1_PIED_Porometer
Phase1_PIED_Porometer_NA[Phase1_PIED_Porometer_NA == "X"] <- NA


####IMPORTANT we don't want integers here! because Porometer readings have decimals
#make sure that columns which previously had X are not characters, but "double" instead
#make changes across all columns using "across()" function
Phase1_PIED_Porometer_NA <- Phase1_PIED_Porometer_NA %>% 
  mutate(across(starts_with("week"), as.double))

#view structure of data, make sure numbers are not characters
str(Phase1_PIED_Porometer_NA)


#I want to rearrange my data to have columns represent Week, Species, speciesID,and Variable
#pivot my data so that weeks become columns.
Phase1_PIED_Porometer_NA_pivot <- tidyr::pivot_longer(Phase1_PIED_Porometer_NA,
                                                   cols = starts_with("week"),
                                                   names_to = "Week",
                                                   values_to = "Porometer",
                                                   names_prefix = "week_")

#round data
round(Phase1_PIED_Porometer_NA_pivot$Porometer, digits=1)



###########PERCENTBROWN

#Replace X with NA in all columns
Phase1_PIED_PercentBrown_NA <- Phase1_PIED_PercentBrown
Phase1_PIED_PercentBrown_NA[Phase1_PIED_PercentBrown_NA == "X"] <- NA


####all percents are read in a characters.... Must change this
#make sure that columns which previously had X are not characters, but "integer" instead
#make changes across all columns using "across()" function
#data with % is considered character string
Phase1_PIED_PercentBrown_NA <- Phase1_PIED_PercentBrown_NA %>% 
  mutate(across(.cols=starts_with("week"),.fns=str_remove, pattern="%")) %>%
  mutate(across(starts_with("week"), as.integer))

#view structure of data, make sure numbers are not characters
str(Phase1_PIED_PercentBrown_NA)


#I want to rearrange my data to have columns represent Week, Species, speciesID,and Variable
#pivot my data so that weeks become columns.
Phase1_PIED_PercentBrown_NA_pivot <- tidyr::pivot_longer(Phase1_PIED_PercentBrown_NA,
                                                      cols = starts_with("week"),
                                                      names_to = "Week",
                                                      values_to = "PercentBrown",
                                                      names_prefix = "week_")

###########DEAD

#No Xs in Dead data, but make new df to manipulate
Phase1_PIED_Dead_NA <- Phase1_PIED_Dead


#view structure of data, should be characters
str(Phase1_PIED_Dead_NA)


#I want to rearrange my data to have columns represent Week, Species, speciesID,and Variable
#pivot my data so that weeks become columns.
Phase1_PIED_Dead_NA_pivot <- tidyr::pivot_longer(Phase1_PIED_Dead_NA,
                                                         cols = starts_with("week"),
                                                         names_to = "Week",
                                                         values_to = "Dead",
                                                         names_prefix = "week_")

#now how can I stitch together these data frames?
#Phase1_PIED_Dead_NA_pivot
#Phase1_PIED_PercentBrown_NA_pivot
#Phase1_PIED_Porometer_NA_pivot
#Phase1_PIED_Weight_NA_pivot


#I think you can only merge 2 at a time
#start with Weight & Porometer
#make sure all data gets in, given that only some species have porometer readings
#make sure 1078 obs at least is retained for combined data. do this with all=true
#looks like when we merge by "SpeciesID", data from Weight doesn't transfer completely
Phase1_PIED_1 <- merge(Phase1_PIED_Weight_NA_pivot,
                       Phase1_PIED_Porometer_NA_pivot,
                       by=c("Species","SpeciesID","Week"), all=TRUE)


#next merge PercentBrown and Dead
#these have half-weeks recorded, make sure these show up
#working! make sure 2156 obs is retained for combined data
Phase1_PIED_2 <- merge(Phase1_PIED_PercentBrown_NA_pivot,
                       Phase1_PIED_Dead_NA_pivot,
                       by=c("Species","SpeciesID","Week"))

#now merge all together
#YAY working!!
Phase1_PIED <- merge(Phase1_PIED_1,
                     Phase1_PIED_2,
                     by=c("Species","SpeciesID","Week"), all=TRUE)

#temp
Phase1_PIED_PIEN <- merge(Phase1_PIED,
                          Phase1_PIEN_Dead_NA_pivot,
                          by=c("Species","SpeciesID","Week", "Dead"), all=TRUE)

#finally, make a CSV!
write.csv(Phase1_PIED, "data_raw/plant_data_2/Phase1_PIED.csv", quote = FALSE, row.names = FALSE)


