
#load packages
library(tidyverse)

################################################################################
#useful functions for analysis
################################################################################

# mode
mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

# Standard Error (SE)
# SE = sd / sqrt(n)
se <- function(x) {
  StandardError <- sd(x)/sqrt(length(x))
  StandardError
}

# Coefficient of Variation (CV)
# CV = sd / mean
cv <- function(x) {
  CoeffVar <- (sd(x)/mean(x)) * 100
  CoeffVar
}


# attributes of data for statistical analysis
alpha <- 0.05
tails <- 2


# fundamental calculations
n     <- length(data)
df    <- length(data) - 1
mean <- mean(data)
median <- median(data)
mode <- mode(data)
var <- var(data)       #var = sd^2
SD <- sd(data)         #SD = 
SE <- se(data)         #SE = sd / sqrt(n)
CV <- cv(data)         #CV = sd / mean; as a percent (unitless)

# Calculating Confidence Intervals (CI)
t     <- abs(qt(1 - (alpha/tails), df))     # abs() ensures the value is positive
CI <- c(mean - (t * SE), mean + (t * SE))   # CI = mean +/- (t * SE)


################################################################################

#read csv
heatwave <- read.csv("data_analysis/Dead_Week.csv")

heatwave_summary <- heatwave %>% 
  group_by(Species, Treatment_temp) %>% 
  summarize(n = length(Dead_Week),
            df = length(Dead_Week) - 1,
            mean = round(mean(Dead_Week), digits = 1),
            median = round(median(Dead_Week), digits = 1),
            mode = round(mode(Dead_Week), digits = 1),
            var = round(var(Dead_Week), digits = 2),
            SD = round(sd(Dead_Week), digits = 2),
            SE = round(se(Dead_Week), digits = 2),
            CV = round(cv(Dead_Week), digits = 2),
            t = abs(qt(1 - (alpha/tails), (length(Dead_Week) - 1))))

heatwave_summary <- heatwave_summary %>% 
  mutate(CI_lower = mean - (t * SE),
         CI_upper = mean + (t * SE))


#save csv
write.csv(heatwave_summary, "data_analysis/heatwave_summary.csv", quote = FALSE, row.names = FALSE)















