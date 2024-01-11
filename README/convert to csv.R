
#load tidyverse
library(tidyverse)

write.csv(Metadata_CollectedValues, "README/Metadata_CollectedValues.csv", quote = FALSE, row.names = FALSE)
write.csv(Metadata_CalculatedValues, "README/Metadata_CalculatedValues.csv", quote = FALSE, row.names = FALSE)


Metadata_CollectedValues <- read_csv(file = "README/Metadata_CollectedValues.csv")
Metadata_CalculatedValues <- read_csv(file = "README/Metadata_CalculatedValues.csv")
