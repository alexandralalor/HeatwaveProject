#Data wrangling script - Phase 1 porometer
#Allie Lalor
#allielalor@gmail.com
#First created: 2022-06-11
#Last updated: 2022-06-11

#load packages
library(tidyverse)

#read csv
porometer_August_26_2021 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 30-Aug-2021.csv")
porometer_September_02_2021 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 3-Sep-2021.csv")
porometer_September_09_2021 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 10-Sep-2021.csv")
porometer_September_16_2021 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 17-Sep-2021.csv")
porometer_September_24_2021 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 23-Sep-2021.csv")
porometer_September_30_2021 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 1-Oct-2021.csv")
porometer_October_07_2021 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 8-Oct-2021.csv")
porometer_October_15_2021 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 14-Oct-2021.csv")
porometer_October_21_2021 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 21-Oct-2021.csv")
porometer_October_29_2021 <-read.csv(file = "data_raw/porometer/SC-1 Porometer Data 29-Oct-2021.csv")
porometer_November_05_2021 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 4-Nov-2021.csv")
porometer_November_11_2021 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 12-Nov-2021.csv")
porometer_November_19_2021 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 18-Nov-2021.csv")
porometer_November_26_2021 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 23-Nov-2021.csv")
porometer_December_03_2021 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 2-Dec-2021.csv")
porometer_December_10_2021 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 9-Dec-2021.csv")
porometer_December_17_2021 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 16-Dec-2021.csv")
porometer_December_23_2021 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 23-Dec-2021.csv")
porometer_January_13_2022 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 14-Jan-2022.csv")
porometer_January_21_2022 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 20-Jan-2022.csv")
porometer_January_28_2022 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 27-Jan-2022.csv")
porometer_February_04_2022 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 3-Feb-2022.csv")
porometer_February_11_2022 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 10-Feb-2022.csv")
porometer_February_18_2022 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 17-Feb-2022.csv")
porometer_February_24_2022 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 24-Feb-2022.csv")
porometer_March_03_2022 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 3-Mar-2022.csv")
porometer_March_11_2022 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 11-Mar-2022.csv")
porometer_March_18_2022 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 18-Mar-2022.csv")
porometer_March_24_2022 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 24-Mar-2022.csv")
porometer_March_31_2022 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 31-Mar-2022.csv")
porometer_April_07_2022 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 7-Apr-2022.csv")
porometer_April_15_2022 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 15-Apr-2022.csv")
porometer_April_21_2022 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 21-Apr-2022.csv")
porometer_April_29_2022 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 29-Apr-2022.csv")
porometer_May_06_2022 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 6-May-2022.csv")
porometer_May_12_2022 <- read.csv(file = "data_raw/porometer/SC-1 Porometer Data 12-May-2022.csv")

#combine all dates
porometer_all <- rbind(porometer_August_26_2021,
                      porometer_September_02_2021, porometer_September_09_2021, porometer_September_16_2021, porometer_September_24_2021, porometer_September_30_2021,
                      porometer_October_07_2021, porometer_October_15_2021, porometer_October_21_2021, porometer_October_29_2021,
                      porometer_November_05_2021, porometer_November_11_2021, porometer_November_19_2021, porometer_November_26_2021,
                      porometer_December_03_2021, porometer_December_10_2021, porometer_December_17_2021, porometer_December_23_2021,
                      porometer_January_13_2022, porometer_January_21_2022, porometer_January_28_2022,
                      porometer_February_04_2022, porometer_February_11_2022, porometer_February_18_2022, porometer_February_24_2022,
                      porometer_March_03_2022, porometer_March_11_2022, porometer_March_18_2022, porometer_March_24_2022, porometer_March_31_2022,
                      porometer_April_07_2022, porometer_April_15_2022, porometer_April_21_2022, porometer_April_29_2022,
                      porometer_May_06_2022, porometer_May_12_2022)




#split up date and time

porometer_all <- porometer_all %>% 
  separate(Time, sep = " ",
           into = c("Date", "Time", "AMPM")) %>% 
  unite("Time", Time:AMPM) %>% 
  mutate(Date = parse_datetime(Date,
                               format = "%m/%d/%Y"))

#check labels
porometer_all %>% 
  summarize(Date = unique(Date))

#save as csv
write.csv(porometer_all, "data_clean/Phase1_Porometer.csv", quote = FALSE, row.names = FALSE)





