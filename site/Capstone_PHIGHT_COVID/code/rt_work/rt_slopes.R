## This document is to create plots relating to the slope of Rt 
## Author: Julia Keating 04/18/2022

# load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(latex2exp)
theme_set(theme_classic())

# read in data
rt <- read.csv("../Data/Processed_Data/Rt/Rt_by_county.csv")
rt$date <- as.Date(rt$date)

series.data <- read.csv("../Data/Processed_data/deaths_teaching_mobility_posture.csv") 

counties <- unique(series.data$county)


# calculate the slope of Rt for each county over weeks 1-3 and weeks 4-6 (starting 09/15/2020)
slopes <- data.frame(county=character(), 
                     first_slope.d=numeric(), second_slope.d=numeric(), 
                     first_slope.c=numeric(), second_slope.c=numeric())
for (c in counties) {
  c.data <- dplyr::filter(rt, county==c)
  first_int <- dplyr::filter(c.data, date>="2020-09-15" & date<="2020-10-05")
  second_int <- dplyr::filter(c.data, date>="2020-10-06" & date<="2020-10-26")
  first_slope.c <- unname((lm(R.cases~date, data=first_int))$coefficients[2])
  first_slope.d <- unname((lm(R.deaths~date, data=first_int))$coefficients[2])
  second_slope.c <- unname((lm(R.cases~date, data=second_int))$coefficients[2])
  second_slope.d <- unname((lm(R.deaths~date, data=second_int))$coefficients[2])
  new_data <- data.frame(county=c,
                         first_slope.d=first_slope.d, second_slope.d=second_slope.d,
                         first_slope.c=first_slope.c, second_slope.c=second_slope.c)
  slopes <- rbind(slopes, new_data)
}

postures <- unique(series.data[,c("county","major_teaching_start", "population_density", "metropolitan_status", "county_enroll", "posture_change")])

slopes2 <- merge(slopes, postures, by=c("county")) %>%
  pivot_longer(cols=c(first_slope.c, second_slope.c, first_slope.d, second_slope.d), values_to="slope", names_to="interval") 
  
# boxplots of slopes by teaching posture for both cases and deaths
ggplot(data=dplyr::filter(slopes2, interval=="first_slope.c" | interval=="second_slope.c"), 
       aes(x=major_teaching_start, y=slope)) +
  geom_boxplot(aes(fill=interval)) +
  labs(x="Starting teaching posture", y="Rate of change of Rt from cases", fill="Weeks, starting 10/1") +
  scale_fill_manual(labels = c("1-3", "4-6"), values = c("#F8766D", "#00BFC4"))

ggplot(data=dplyr::filter(slopes2, interval=="first_slope.d" | interval=="second_slope.d"), 
       aes(x=major_teaching_start, y=slope)) +
  geom_boxplot(aes(fill=interval)) +
  labs(x="Starting teaching posture", y="Rate of change of Rt from deaths", fill="Weeks, starting 10/1") +
  scale_fill_manual(labels = c("1-3", "4-6"), values = c("#F8766D", "#00BFC4"))

# boxplots of slopes by metro status for both cases and deaths
ggplot(data=dplyr::filter(slopes2, interval=="first_slope.c" | interval=="second_slope.c"), 
       aes(x=metropolitan_status, y=slope)) +
  geom_boxplot(aes(fill=interval)) +
  labs(x="Metropolitan status", y="Rate of change of Rt from cases", fill="Weeks, starting 10/1") +
  scale_fill_manual(labels = c("1-3", "4-6"), values = c("#F8766D", "#00BFC4"))

ggplot(data=dplyr::filter(slopes2, interval=="first_slope.d" | interval=="second_slope.d"), 
       aes(x=metropolitan_status, y=slope)) +
  geom_boxplot(aes(fill=interval)) +
  labs(x="Metropolitan status", y="Rate of change of Rt from deaths", fill="Weeks, starting 10/1") +
  scale_fill_manual(labels = c("1-3", "4-6"), values = c("#F8766D", "#00BFC4"))

# plot slope against log(pop density)
ggplot(data=dplyr::filter(slopes2, interval=="first_slope.c" | interval=="second_slope.c"), 
       aes(x=log(population_density), group=interval)) +
  geom_point(aes(y=slope, colour=interval)) +
  geom_smooth(aes(x=log(population_density), y=slope, colour=interval), method="lm")

ggplot(data=dplyr::filter(slopes2, interval=="first_slope.d" | interval=="second_slope.d"), 
       aes(x=log(population_density), group=interval)) +
  geom_point(aes(y=slope, colour=interval)) +
  geom_smooth(aes(x=log(population_density), y=slope, colour=interval), method="lm")

# plot slope total
ggplot(data=dplyr::filter(slopes2, interval=="first_slope.c" | interval=="second_slope.c"), 
       aes(x=interval, y=slope)) +
  geom_boxplot() +
  labs(x="Weeks", y="Rate of change of Rt from cases") 

ggplot(data=dplyr::filter(slopes2, interval=="first_slope.d" | interval=="second_slope.d"), 
       aes(x=interval, y=slope)) +
  geom_boxplot() +
  labs(x="Weeks", y="Rate of change of Rt from deaths") 

# plot slope against slope
slopes <- merge(slopes, postures, by=c("county")) 

ggplot(data=slopes, aes(x=first_slope.d, y=second_slope.d)) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  geom_abline(intercept=0, slope=1, colour="gray", lty=2) +
  geom_point(aes(colour=major_teaching_start)) +
  facet_wrap(~major_teaching_start) +
  labs(x=TeX("Slope of $R_t$ in Period 1"), y=TeX("Slope of $R_t$ in Period 2"), colour="Majority starting teaching posture")

# plot slope against county enroll
ggplot(data=dplyr::filter(slopes2, interval=="first_slope.c" | interval=="second_slope.c"), 
       aes(x=log(county_enroll), group=interval)) +
  geom_point(aes(y=slope, colour=interval)) +
  geom_smooth(aes(x=log(county_enroll), y=slope, colour=interval), method="lm")

ggplot(data=dplyr::filter(slopes2, interval=="first_slope.d" | interval=="second_slope.d"), 
       aes(x=log(county_enroll), group=interval)) +
  geom_point(aes(y=slope, colour=interval)) +
  geom_smooth(aes(x=log(county_enroll), y=slope, colour=interval), method="lm")


