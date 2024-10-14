## This document is to create visualizations of the corrected case time series 
## Author: Julia Keating 03/30/2022

## load libraries
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(janitor)
library(readxl)
theme_set(theme_classic())

# read in data
series <- read.csv("../Data/Processed_Data/deaths_teaching_mobility_posture.csv")
counties <- unique(series$county)
series$date <- as.Date(series$date)

# Looking at original case time series vs 7-day rolling median for all counties  
cols <- c("Original"="black", "7-day Rolling Median"="blue")

ggplot(filter(series, county %in% counties[1:24]), aes(x=date, group=county)) +
  geom_ribbon(aes(ymin = ylower, ymax = yupper), fill = "light gray") +
  geom_line(aes(y=newconfirmed, colour="Original")) +
  geom_line(aes(y=rmed_cases, colour="7-day Rolling Median")) +
  facet_wrap(~county, nrow = 4) +
  labs(y="Cases", x="Date") +
  scale_color_manual(name='Data Version', values=cols)
ggplot(filter(series, county %in% counties[25:48]), aes(x=date, group=county)) +
  geom_ribbon(aes(ymin = ylower, ymax = yupper), fill = "light gray") +
  geom_line(aes(y=newconfirmed, colour="Original")) +
  geom_line(aes(y=rmed_cases, colour="7-day Rolling Median")) +
  facet_wrap(~county, nrow = 4) +
  labs(y="Cases", x="Date") +
  scale_color_manual(name='Data Version', values=cols)
ggplot(filter(series, county %in% counties[49:72]), aes(x=date, group=county)) +
  geom_ribbon(aes(ymin = ylower, ymax = yupper), fill = "light gray") +
  geom_line(aes(y=newconfirmed, colour="Original")) +
  geom_line(aes(y=rmed_cases, colour="7-day Rolling Median")) +
  facet_wrap(~county, nrow = 4) +
  labs(y="Cases", x="Date") +
  scale_color_manual(name='Data Version', values=cols)
ggplot(filter(series, county %in% counties[73:86]), aes(x=date, group=county)) +
  geom_ribbon(aes(ymin = ylower, ymax = yupper), fill = "light gray") +
  geom_line(aes(y=newconfirmed, colour="Original")) +
  geom_line(aes(y=rmed_cases, colour="7-day Rolling Median")) +
  facet_wrap(~county, nrow = 4) +
  labs(y="Cases", x="Date") +
  scale_color_manual(name='Data Version', values=cols)

# Looking at original vs corrected case time series for each county
cols <- c("Original"="black", "Corrected"="red")

for (c in counties){
  temp_plot <- ggplot(filter(series, county==c), aes(x=date)) +
    geom_ribbon(aes(ymin = ylower, ymax = yupper), fill = "light gray") +
    geom_line(aes(y=newconfirmed, colour="Original")) +
    geom_line(aes(y=rev_newconfirmed2, colour="Corrected")) +
    labs(y="Cases", x="Date") +
    ggtitle(c) +
    scale_color_manual(name='Data Version', values=cols) 
  
  # un-comment the line below to save the plots as images
  #ggsave(plot=temp_plot, filename=paste0("plot_", c,".png"), path="corrected_plots", width = 35, height = 18, units = "cm")
}

# Aggregating county-level series to the state-level
series_total <- series %>%
  group_by(date) %>%
  summarize(newconfirmed=sum(newconfirmed),rev_newconfirmed2=sum(rev_newconfirmed2),
            newdeaths=sum(newdeaths))

cols <- c("Original"="light gray", "Corrected"="black")

ggplot(series_total, aes(x=date)) +
  geom_line(aes(y=newconfirmed, colour="Original")) +
  geom_line(aes(y=rev_newconfirmed2, colour="Corrected")) +
  labs(y="Total Cases", x="Date") +
  scale_color_manual(name='Data Version', values=cols) 

# Aggregating county-level series to the state-level and comparing to state-level case series
series_total <- series %>%
  group_by(date) %>%
  summarize(county_newconfirmed=sum(newconfirmed),rev_newconfirmed2=sum(rev_newconfirmed2))

state_data <- read_excel("../Data/RawData/OH_ST_COVID_CASES_20220330_pop.xlsx") %>%
  clean_names()
state_data$date <- mdy(state_data$date)
series_total <- merge(series_total, state_data[,c(3,7)], by="date") 

cols=c("State-level"="red", "Aggregated county-level before processing"="black")
ggplot(data=series_total, aes(x=date)) +
  geom_line(aes(y=newconfirmed, colour="State-level")) +
  geom_line(aes(y=county_newconfirmed, colour="Aggregated county-level before processing")) +
  labs(y="Total Cases", x="Date") +
  scale_color_manual(name='Data Version', values=cols) 

cols=c("State-level"="red", "Aggregated county-level after processing"="black")
ggplot(data=series_total, aes(x=date)) +
  geom_line(aes(y=newconfirmed, colour="State-level")) +
  geom_line(aes(y=rev_newconfirmed2, colour="Aggregated county-level after processing")) +
  labs(y="Total Cases", x="Date") +
  scale_color_manual(name='Data Version', values=cols) 

## Looking at original vs corrected case series for the 9 largest counties
cols <- c("Original"="black", "Corrected"="red")

counties <- unique(series[,c("county","population")]) %>%
  arrange(desc(population))
counties <- counties[1:9,]$county
counties <- paste0(str_to_title(counties)," County")
series$county <- paste0(str_to_title(series$county)," County")

ggplot(data=filter(series, county %in% counties), aes(x=date, group=county)) +
  geom_ribbon(aes(ymin = ylower, ymax = yupper), fill = "light gray") +
  geom_line(aes(y=newconfirmed, colour="Original")) +
  geom_line(aes(y=rev_newconfirmed2, colour="Corrected")) +
  labs(y="Cases", x="Date") +
  facet_wrap(~county, nrow=3) +
  scale_color_manual(name='Data Version', values=cols) 

## 2 weird counties
ggplot(data=filter(series, county=="Marion County" | county=="Pickaway County"), aes(x=date, group=county)) +
  geom_ribbon(aes(ymin = ylower, ymax = yupper), fill = "light gray") +
  geom_line(aes(y=newconfirmed, colour="Original")) +
  geom_line(aes(y=rev_newconfirmed2, colour="Corrected")) +
  labs(y="Cases", x="Date") +
  facet_wrap(~county, nrow=1) +
  scale_color_manual(name='Data Version', values=cols) 
