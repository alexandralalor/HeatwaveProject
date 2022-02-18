#Data wrangling script - Phase 1
#ARL Feb 2022

#working directory
setwd("~/Desktop/UofA/HW project/analysis/HeatwaveProject")

#load tidyverse
library(tidyverse)

#read in data
Phase1_Chamber1_Dates <- read.csv(file = "data_raw/Phase1_Chamber1_Dates.csv")
Phase1_InitialData <- read.csv(file = "data_raw/Phase1_InitialData.csv")