
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
tails <- 1


# fundamental calculations
n     <- length(data)
df    <- length(data) - 1
mean <- mean(data)
median <- median(data)
mode <- mode(data)
var <- var(data)       #var = sd^2
SD <- sd(data)         #SD = sqrt(var)
SE <- se(data)         #SE = sd / sqrt(n)
CV <- cv(data)         #CV = sd / mean; as a percent (unitless)

# Calculating Confidence Intervals (CI)
t.critical <- abs(qt(1 - (alpha/tails), df))  # abs() ensures the value is positive
half_width <- t.critical * SE
CI_upper <- mean + half_width
CI_lower <- mean - half_width
CI <- c(mean - (t.critical * SE), mean + (t.critical * SE))   # CI = mean +/- (t * SE)


################################################################################

#read csv
heatwave <- read.csv("data_analysis/Dead_Week.csv")

# heatwave_filter <- heatwave %>% 
#   filter(SpeciesID != "PIFL16")

heatwave_summary_test <- heatwave %>% 
  group_by(Species, Treatment_temp) %>% 
  summarize(n = length(Dead_Week),
            df = length(Dead_Week) - 1,
            mean = round(mean(Dead_Week), digits = 1),
            median = round(median(Dead_Week), digits = 1),
            mode = round(mode(Dead_Week), digits = 1),
            Q1 = round(quantile(Dead_Week, probs = 0.25), digits = 1),
            Q3 = round(quantile(Dead_Week, probs = 0.75), digits = 1),
            var = round(var(Dead_Week), digits = 2),
            SD = round(sd(Dead_Week), digits = 2),
            SE = round(se(Dead_Week), digits = 2),
            #CV = round(cv(Dead_Week), digits = 2),
            t.critical = round(abs(qt(1 - (alpha/tails), (length(Dead_Week) - 1))), digits = 3))

heatwave_summary_test <- heatwave_summary_test %>% 
  mutate(CI_lower_mean = round(mean - (t.critical * SE), digits = 2),
         CI_upper_mean = round(mean + (t.critical * SE), digits = 2),
         CI_lower_med = round(median - (t.critical * SE), digits = 2),
         CI_upper_med = round(median + (t.critical * SE), digits = 2),
         CI_lower_Q1 = round(Q1 - (t.critical * SE), digits = 2),
         CI_upper_Q1 = round(Q1 + (t.critical * SE), digits = 2))


#save csv
write.csv(heatwave_summary, "data_analysis/heatwave_summary.csv", quote = FALSE, row.names = FALSE)
#write.csv(heatwave_summary, "data_analysis/heatwave_summary_filter.csv", quote = FALSE, row.names = FALSE)


################################################################################
# Find pooled SD
heatwave_summary <- read_csv("data_analysis/heatwave_summary.csv")

# #PIPO
# amb <- heatwave_summary %>% 
#   filter(Species == "PIPO", Treatment_temp == "Ambient")
# hw <- heatwave_summary %>% 
#   filter(Species == "PIPO", Treatment_temp == "Ambient_HW")
# #PIED
# amb <- heatwave_summary %>%
#   filter(Species == "PIED", Treatment_temp == "Ambient")
# hw <- heatwave_summary %>%
#   filter(Species == "PIED", Treatment_temp == "Ambient_HW")
# #PIFL
# amb <- heatwave_summary %>%
#   filter(Species == "PIFL", Treatment_temp == "Ambient")
# hw <- heatwave_summary %>%
#   filter(Species == "PIFL", Treatment_temp == "Ambient_HW")
# #PSME
# amb <- heatwave_summary %>%
#   filter(Species == "PSME", Treatment_temp == "Ambient")
# hw <- heatwave_summary %>%
#   filter(Species == "PSME", Treatment_temp == "Ambient_HW")
# #PIEN
# amb <- heatwave_summary %>%
#   filter(Species == "PIEN", Treatment_temp == "Ambient")
# hw <- heatwave_summary %>%
#   filter(Species == "PIEN", Treatment_temp == "Ambient_HW")

amb.df <- amb$df
hw.df <- hw$df
amb.var <- amb$var
hw.var <- hw$var

#numerator = (amb.df*amb.var) + (hw.df*hw.var)
#denominator = amb.df + hw.df
#SD_pooled = sqrt(numerator/denominator)
# SD_pooled_PIPO <- sqrt(((amb.df*amb.var) + (hw.df*hw.var)) / (amb.df + hw.df))
# SD_pooled_PIED <- sqrt(((amb.df*amb.var) + (hw.df*hw.var)) / (amb.df + hw.df))
# SD_pooled_PIFL <- sqrt(((amb.df*amb.var) + (hw.df*hw.var)) / (amb.df + hw.df))
# SD_pooled_PSME <- sqrt(((amb.df*amb.var) + (hw.df*hw.var)) / (amb.df + hw.df))
# SD_pooled_PIEN <- sqrt(((amb.df*amb.var) + (hw.df*hw.var)) / (amb.df + hw.df))

SD_pooled_df <- data.frame(Species = c("PIPO", "PIED", "PIFL", "PSME", "PIEN"),
                        SD_pool = c(SD_pooled_PIPO, SD_pooled_PIED, SD_pooled_PIFL, SD_pooled_PSME, SD_pooled_PIEN))

heatwave_summary <- merge(heatwave_summary, SD_pooled_df, by = "Species")


#save csv
write.csv(heatwave_summary, "data_analysis/heatwave_summary.csv", quote = FALSE, row.names = FALSE)


################################################################################
# Find pooled SE
heatwave_summary <- read_csv("data_analysis/heatwave_summary.csv")

# #PIPO
# amb <- heatwave_summary %>%
#   filter(Species == "PIPO", Treatment_temp == "Ambient")
# hw <- heatwave_summary %>%
#   filter(Species == "PIPO", Treatment_temp == "Ambient_HW")
# #PIED
# amb <- heatwave_summary %>%
#   filter(Species == "PIED", Treatment_temp == "Ambient")
# hw <- heatwave_summary %>%
#   filter(Species == "PIED", Treatment_temp == "Ambient_HW")
#PIFL
# amb <- heatwave_summary %>%
#   filter(Species == "PIFL", Treatment_temp == "Ambient")
# hw <- heatwave_summary %>%
#   filter(Species == "PIFL", Treatment_temp == "Ambient_HW")
# #PSME
# amb <- heatwave_summary %>%
#   filter(Species == "PSME", Treatment_temp == "Ambient")
# hw <- heatwave_summary %>%
#   filter(Species == "PSME", Treatment_temp == "Ambient_HW")
# #PIEN
# amb <- heatwave_summary %>%
#   filter(Species == "PIEN", Treatment_temp == "Ambient")
# hw <- heatwave_summary %>%
#   filter(Species == "PIEN", Treatment_temp == "Ambient_HW")

amb.n <- amb$n
hw.n <- hw$n
SD_pool <- amb$SD_pool


#SE_pool = SD_pool * sqrt(1/n1 + 1/n2)
# SE_pooled_PIPO <- SD_pool * sqrt((1/amb.n) + (1/hw.n))
# SE_pooled_PIED <- SD_pool * sqrt((1/amb.n) + (1/hw.n))
# SE_pooled_PIFL <- SD_pool * sqrt((1/amb.n) + (1/hw.n))
# SE_pooled_PSME <- SD_pool * sqrt((1/amb.n) + (1/hw.n))
# SE_pooled_PIEN <- SD_pool * sqrt((1/amb.n) + (1/hw.n))


SE_pooled_df <- data.frame(Species = c("PIPO", "PIED", "PIFL", "PSME", "PIEN"),
                           SE_pool = c(SE_pooled_PIPO, SE_pooled_PIED, SE_pooled_PIFL, SE_pooled_PSME, SE_pooled_PIEN))

heatwave_summary <- merge(heatwave_summary, SE_pooled_df, by = "Species")

#save csv
write.csv(heatwave_summary, "data_analysis/heatwave_summary.csv", quote = FALSE, row.names = FALSE)

################################################################################
# t-test
heatwave_summary <- read_csv("data_analysis/heatwave_summary.csv")


# #PIPO
# amb <- heatwave_summary %>%
#   filter(Species == "PIPO", Treatment_temp == "Ambient")
# hw <- heatwave_summary %>%
#   filter(Species == "PIPO", Treatment_temp == "Ambient_HW")
# #PIED
# amb <- heatwave_summary %>%
#   filter(Species == "PIED", Treatment_temp == "Ambient")
# hw <- heatwave_summary %>%
#   filter(Species == "PIED", Treatment_temp == "Ambient_HW")
# #PIFL
# amb <- heatwave_summary %>%
#   filter(Species == "PIFL", Treatment_temp == "Ambient")
# hw <- heatwave_summary %>%
#   filter(Species == "PIFL", Treatment_temp == "Ambient_HW")
# #PSME
# amb <- heatwave_summary %>%
#   filter(Species == "PSME", Treatment_temp == "Ambient")
# hw <- heatwave_summary %>%
#   filter(Species == "PSME", Treatment_temp == "Ambient_HW")
# #PIEN
# amb <- heatwave_summary %>%
#   filter(Species == "PIEN", Treatment_temp == "Ambient")
# hw <- heatwave_summary %>%
#   filter(Species == "PIEN", Treatment_temp == "Ambient_HW")


# drought only = mean_o
# drought + HW = mean
mean_o <- amb$Q1
mean <- hw$Q1
SE_pool <- amb$SE_pool
df <- hw$df


## t-statistic = [(mean - mean_o) - 0] / SE_pool
## find the associated p-value given t statistic

# t.stat_PIPO <- (mean - mean_o) - 0 / SE_pool
# p_PIPO <- (pt(-abs(t.stat_PIPO), df))

# t.stat_PIED <- (mean - mean_o) - 0 / SE_pool
# p_PIED <- (pt(-abs(t.stat_PIED), df))

# t.stat_PIFL <- (mean - mean_o) - 0 / SE_pool
# p_PIFL <- (pt(-abs(t.stat_PIFL), df))

# t.stat_PSME <- (mean - mean_o) - 0 / SE_pool
# p_PSME <- (pt(-abs(t.stat_PSME), df))

# t.stat_PIEN <- (mean - mean_o) - 0 / SE_pool
# p_PIEN <- (pt(-abs(t.stat_PIEN), df))


stats_df <- data.frame(Species = c("PIPO", "PIED", "PIFL", "PSME", "PIEN"),
                       t.stat_Q1 = c(t.stat_PIPO, t.stat_PIED, t.stat_PIFL, t.stat_PSME, t.stat_PIEN),
                       p_Q1 = c(p_PIPO, p_PIED, p_PIFL, p_PSME, p_PIEN))

heatwave_summary <- merge(heatwave_summary, stats_df, by = "Species")

#save csv
write.csv(heatwave_summary, "data_analysis/heatwave_summary.csv", quote = FALSE, row.names = FALSE)















