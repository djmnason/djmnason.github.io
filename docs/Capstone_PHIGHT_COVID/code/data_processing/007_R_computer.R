## This document is to compute the estimated reproduction number R for each county in Ohio 
## using both case and death time series
## Author: Julia Keating 03/4/2022

## load libraries
library(tidyverse)
library(EpiEstim)
library(dplyr)
library(ggplot2)
library(ggh4x)

series.data <- read.csv("../Data/Processed_Data/deaths_teaching_mobility_posture.csv")
# map negative case numbers to 0, round decimals and convert to integers
series.data$rev_newconfirmed2 <- as.integer(round(ifelse(series.data$rev_newconfirmed2<0, 0, series.data$rev_newconfirmed2), digits=0))

## by county

counties = unique(series.data$county)
series.county <- series.data

new_data = data.frame(county=character(), date=character(), 
                      R.deaths=numeric(), R.deaths.q2.5=numeric(), R.deaths.q97.5=numeric(),
                      R.cases=numeric(), R.cases.q2.5=numeric(), R.cases.q97.5=numeric())
for (c in counties) {
  c.data <- dplyr::filter(series.county, county==c)[,c(2,12,14)]
  
  # estimate Rt for each county using time series of deaths:
  config.d = make_config(incid = c.data$rev_newdeaths, t_start = c(2:(nrow(c.data)-13)),
                         t_end =  c(15:nrow(c.data)), method="non_parametric_si", 
                         si_distr=dgamma(0:1000, shape=(1/0.4)^2, rate=1/(23.9*(0.4^2)))) ## Gamma(mean=23.9, coefficient of variation=0.4)
  R.d <- estimate_R(incid=c.data$rev_newdeaths, method="non_parametric_si", config=config.d)
  
  # estimate Rt for each county using time series of cases:
  config.c = make_config(incid = c.data$rev_newconfirmed2, 
                         method="non_parametric_si", t_start = c(2:(nrow(c.data)-13)),
                         t_end =  c(15:nrow(c.data)),
                         si_distr=dgamma(0:1000, shape=(1/0.6)^2, rate=1/(15.2*(0.6^2)))) ## Gamma(mean=10.2+5, coefficient of variation=0.6)
  R.c <- estimate_R(incid=c.data$rev_newconfirmed2, method="non_parametric_si", config=config.c)
  
  counties.R <- data.frame(county = rep(c,times=length(R.d$R$`Mean(R)`)),date = c.data$date[15:length(c.data$date)], 
                           R.deaths = R.d$R$`Mean(R)`, R.deaths.q2.5 = R.d$R$`Quantile.0.025(R)`, R.deaths.q97.5 = R.d$R$`Quantile.0.975(R)`,
                           R.cases = R.c$R$`Mean(R)`, R.cases.q2.5 = R.c$R$`Quantile.0.025(R)`, R.cases.q97.5 = R.c$R$`Quantile.0.975(R)`)
  new_data <- rbind(new_data, counties.R)
}

write.csv(new_data,"../Data/Processed_Data/Rt/Rt_by_county.csv")

## by starting teaching posture

postures = unique(series.data$major_teaching_start)
series.pos = series.data %>%
  group_by(date, major_teaching_start) %>%
  summarize(rev_newdeaths=sum(rev_newdeaths), rev_newconfirmed2=sum(rev_newconfirmed2))

new_data = data.frame(major_teaching_start=character(), date=character(),
                      R.deaths=numeric(), R.deaths.q2.5=numeric(), R.deaths.q97.5=numeric(),
                      R.cases=numeric(), R.cases.q2.5=numeric(), R.cases.q97.5=numeric())
for (p in postures) {
  p.data <- dplyr::filter(series.pos, major_teaching_start==p)[,c(1,3,4)]
  
  # estimate Rt for each posture using time series of deaths:
  config.d = make_config(incid = p.data$rev_newdeaths, t_start = c(2:(nrow(p.data)-13)),
                         t_end =  c(15:nrow(p.data)), method="non_parametric_si", 
                         si_distr=dgamma(0:1000, shape=(1/0.4)^2, rate=1/(23.9*(0.4^2))))
  R.d <- estimate_R(incid=p.data$rev_newdeaths, method="non_parametric_si", config=config.d)
  
  # estimate Rt for each posture using time series of cases:
  config.c = make_config(incid = p.data$rev_newconfirmed2, 
                         method="non_parametric_si", t_start = c(2:(nrow(p.data)-13)),
                         t_end =  c(15:nrow(p.data)),
                         si_distr=dgamma(0:1000, shape=(1/0.6)^2, rate=1/(15.2*(0.6^2))))
  R.c <- estimate_R(incid=p.data$rev_newconfirmed2, method="non_parametric_si", config=config.c)
  
  postures.R <- data.frame(major_teaching_start = rep(p,times=length(R.d$R$`Mean(R)`)),date = p.data$date[15:length(p.data$date)], 
                           R.deaths = R.d$R$`Mean(R)`, R.deaths.q2.5 = R.d$R$`Quantile.0.025(R)`, R.deaths.q97.5 = R.d$R$`Quantile.0.975(R)`,
                           R.cases = R.c$R$`Mean(R)`, R.cases.q2.5 = R.c$R$`Quantile.0.025(R)`, R.cases.q97.5 = R.c$R$`Quantile.0.975(R)`)
  new_data <- rbind(new_data, postures.R)
}

write.csv(new_data,"../Data/Processed_Data/Rt/Rt_by_starting_posture.csv")

## by change in teaching posture

postures = unique(series.data$posture_change)
series.pos = series.data %>%
  group_by(date, posture_change) %>%
  summarize(rev_newdeaths=sum(rev_newdeaths), rev_newconfirmed2=sum(rev_newconfirmed2))

new_data = data.frame(posture_change=character(), date=character(),
                      R.deaths=numeric(), R.deaths.q2.5=numeric(), R.deaths.q97.5=numeric(),
                      R.cases=numeric(), R.cases.q2.5=numeric(), R.cases.q97.5=numeric())
for (p in postures) {
  p.data <- dplyr::filter(series.pos, posture_change==p)[,c(1,3,4)]
  
  # estimate Rt for each posture using time series of deaths:
  config.d = make_config(incid = p.data$rev_newdeaths, t_start = c(2:(nrow(p.data)-13)),
                         t_end =  c(15:nrow(p.data)), method="non_parametric_si", 
                         si_distr=dgamma(0:1000, shape=(1/0.4)^2, rate=1/(23.9*(0.4^2))))
  R.d <- estimate_R(incid=p.data$rev_newdeaths, method="non_parametric_si", config=config.d)
  
  # estimate Rt for each posture using time series of cases:
  config.c = make_config(incid = p.data$rev_newconfirmed2, 
                         method="non_parametric_si", t_start = c(2:(nrow(p.data)-13)),
                         t_end =  c(15:nrow(p.data)),
                         si_distr=dgamma(0:1000, shape=(1/0.6)^2, rate=1/(15.2*(0.6^2))))
  R.c <- estimate_R(incid=p.data$rev_newconfirmed2, method="non_parametric_si", config=config.c)
  
  postures.R <- data.frame(posture_change = rep(p,times=length(R.d$R$`Mean(R)`)),date = p.data$date[15:length(p.data$date)], 
                           R.deaths = R.d$R$`Mean(R)`, R.deaths.q2.5 = R.d$R$`Quantile.0.025(R)`, R.deaths.q97.5 = R.d$R$`Quantile.0.975(R)`,
                           R.cases = R.c$R$`Mean(R)`, R.cases.q2.5 = R.c$R$`Quantile.0.025(R)`, R.cases.q97.5 = R.c$R$`Quantile.0.975(R)`)
  new_data <- rbind(new_data, postures.R)
}

write.csv(new_data,"../Data/Processed_Data/Rt/Rt_by_posture_change.csv")

## by starting teaching posture for only micropolitan counties
postures = unique(series.data$major_teaching_start)
series.pos = dplyr::filter(series.data, nchs_urban_rural_status=="Micropolitan") %>%
  group_by(date, major_teaching_start) %>%
  summarize(rev_newdeaths=sum(rev_newdeaths), rev_newconfirmed2=sum(rev_newconfirmed2))

new_data = data.frame(major_teaching_start=character(), date=character(),
                      R.deaths=numeric(), R.deaths.q2.5=numeric(), R.deaths.q97.5=numeric(),
                      R.cases=numeric(), R.cases.q2.5=numeric(), R.cases.q97.5=numeric())
for (p in postures) {
  p.data <- dplyr::filter(series.pos, major_teaching_start==p)[,c(1,3,4)]
  
  # estimate Rt for each posture using time series of deaths:
  config.d = make_config(incid = p.data$rev_newdeaths, t_start = c(2:(nrow(p.data)-13)),
                         t_end =  c(15:nrow(p.data)), method="non_parametric_si", 
                         si_distr=dgamma(0:1000, shape=(1/0.4)^2, rate=1/(23.9*(0.4^2))))
  R.d <- estimate_R(incid=p.data$rev_newdeaths, method="non_parametric_si", config=config.d)
  
  # estimate Rt for each posture using time series of cases:
  config.c = make_config(incid = p.data$rev_newconfirmed2, 
                         method="non_parametric_si", t_start = c(2:(nrow(p.data)-13)),
                         t_end =  c(15:nrow(p.data)),
                         si_distr=dgamma(0:1000, shape=(1/0.6)^2, rate=1/(15.2*(0.6^2))))
  R.c <- estimate_R(incid=p.data$rev_newconfirmed2, method="non_parametric_si", config=config.c)
  
  postures.R <- data.frame(major_teaching_start = rep(p,times=length(R.d$R$`Mean(R)`)),date = p.data$date[15:length(p.data$date)], 
                           R.deaths = R.d$R$`Mean(R)`, R.deaths.q2.5 = R.d$R$`Quantile.0.025(R)`, R.deaths.q97.5 = R.d$R$`Quantile.0.975(R)`,
                           R.cases = R.c$R$`Mean(R)`, R.cases.q2.5 = R.c$R$`Quantile.0.025(R)`, R.cases.q97.5 = R.c$R$`Quantile.0.975(R)`)
  new_data <- rbind(new_data, postures.R)
}

write.csv(new_data,"../Data/Processed_Data/Rt/Rt_by_starting_posture_micro.csv")


## by urban rural status

status = unique(series.data$nchs_urban_rural_status)
series.urs = series.data %>%
  group_by(date, nchs_urban_rural_status) %>%
  summarize(rev_newdeaths=sum(rev_newdeaths), rev_newconfirmed2=sum(rev_newconfirmed2))

new_data = data.frame(nchs_urban_rural_status=character(), date=character(),
                      R.deaths=numeric(), R.deaths.q2.5=numeric(), R.deaths.q97.5=numeric(),
                      R.cases=numeric(), R.cases.q2.5=numeric(), R.cases.q97.5=numeric())
for (p in status) {
  p.data <- dplyr::filter(series.urs, nchs_urban_rural_status==p)[,c(1,3,4)]
  
  # estimate Rt for each status using time series of deaths:
  config.d = make_config(incid = p.data$rev_newdeaths, t_start = c(2:(nrow(p.data)-13)),
                         t_end =  c(15:nrow(p.data)), method="non_parametric_si", 
                         si_distr=dgamma(0:1000, shape=(1/0.4)^2, rate=1/(23.9*(0.4^2))))
  R.d <- estimate_R(incid=p.data$rev_newdeaths, method="non_parametric_si", config=config.d)
  
  # estimate Rt for each status using time series of cases:
  config.c = make_config(incid = p.data$rev_newconfirmed2, 
                         method="non_parametric_si", t_start = c(2:(nrow(p.data)-13)),
                         t_end =  c(15:nrow(p.data)),
                         si_distr=dgamma(0:1000, shape=(1/0.6)^2, rate=1/(15.2*(0.6^2))))
  R.c <- estimate_R(incid=p.data$rev_newconfirmed2, method="non_parametric_si", config=config.c)
  
  nchs.R <- data.frame(nchs_urban_rural_status = rep(p,times=length(R.d$R$`Mean(R)`)),date = p.data$date[15:length(p.data$date)], 
                       R.deaths = R.d$R$`Mean(R)`, R.deaths.q2.5 = R.d$R$`Quantile.0.025(R)`, R.deaths.q97.5 = R.d$R$`Quantile.0.975(R)`,
                       R.cases = R.c$R$`Mean(R)`, R.cases.q2.5 = R.c$R$`Quantile.0.025(R)`, R.cases.q97.5 = R.c$R$`Quantile.0.975(R)`)
  new_data <- rbind(new_data, nchs.R)
}

write.csv(new_data,"../Data/Processed_Data/Rt/Rt_by_nchs_urban_rural.csv")

## by metropolitan status

status = unique(series.data$metropolitan_status)
series.ms = series.data %>%
  group_by(date, metropolitan_status) %>%
  summarize(rev_newdeaths=sum(rev_newdeaths), rev_newconfirmed2=sum(rev_newconfirmed2))

new_data = data.frame(metropolitan_status=character(), date=character(),
                      R.deaths=numeric(), R.deaths.q2.5=numeric(), R.deaths.q97.5=numeric(),
                      R.cases=numeric(), R.cases.q2.5=numeric(), R.cases.q97.5=numeric())
for (p in status) {
  p.data <- dplyr::filter(series.ms, metropolitan_status==p)[,c(1,3,4)]
  
  # estimate Rt for each status using time series of deaths:
  config.d = make_config(incid = p.data$rev_newdeaths, t_start = c(2:(nrow(p.data)-13)),
                         t_end =  c(15:nrow(p.data)), method="non_parametric_si", 
                         si_distr=dgamma(0:1000, shape=(1/0.4)^2, rate=1/(23.9*(0.4^2))))
  R.d <- estimate_R(incid=p.data$rev_newdeaths, method="non_parametric_si", config=config.d)
  
  # estimate Rt for each status using time series of cases:
  config.c = make_config(incid = p.data$rev_newconfirmed2, 
                         method="non_parametric_si", t_start = c(2:(nrow(p.data)-13)),
                         t_end =  c(15:nrow(p.data)),
                         si_distr=dgamma(0:1000, shape=(1/0.6)^2, rate=1/(15.2*(0.6^2))))
  R.c <- estimate_R(incid=p.data$rev_newconfirmed2, method="non_parametric_si", config=config.c)
  
  ms.R <- data.frame(metropolitan_status = rep(p,times=length(R.d$R$`Mean(R)`)),date = p.data$date[15:length(p.data$date)], 
                     R.deaths = R.d$R$`Mean(R)`, R.deaths.q2.5 = R.d$R$`Quantile.0.025(R)`, R.deaths.q97.5 = R.d$R$`Quantile.0.975(R)`,
                     R.cases = R.c$R$`Mean(R)`, R.cases.q2.5 = R.c$R$`Quantile.0.025(R)`, R.cases.q97.5 = R.c$R$`Quantile.0.975(R)`)
  new_data <- rbind(new_data, ms.R)
}

write.csv(new_data,"../Data/Processed_Data/Rt/Rt_by_metropolitan.csv")

## total

series.total = series.data %>%
  group_by(date) %>%
  summarize(rev_newdeaths=sum(rev_newdeaths), rev_newconfirmed2=sum(rev_newconfirmed2))

# estimate Rt for using time series of deaths:
config.d = make_config(incid = series.total$rev_newdeaths, t_start = c(2:(nrow(series.total)-13)),
                       t_end =  c(15:nrow(series.total)), method="non_parametric_si", 
                       si_distr=dgamma(0:100, shape=(1/0.4)^2, rate=1/(23.9*(0.4^2))))
R.d <- estimate_R(incid=series.total$rev_newdeaths, method="non_parametric_si", config=config.d)

# estimate Rt for each county using time series of cases:
config.c = make_config(incid = series.total$rev_newconfirmed2, 
                       method="non_parametric_si", t_start = c(2:(nrow(series.total)-13)),
                       t_end =  c(15:nrow(series.total)),
                       si_distr=dgamma(0:100, shape=(1/0.6)^2, rate=1/(15.2*(0.6^2))))
R.c <- estimate_R(incid=series.total$rev_newconfirmed2, method="non_parametric_si", config=config.c)

total.R <- data.frame(date = series.total$date[15:length(series.total$date)], 
                      R.deaths = R.d$R$`Mean(R)`, R.deaths.q2.5 = R.d$R$`Quantile.0.025(R)`, R.deaths.q97.5 = R.d$R$`Quantile.0.975(R)`,
                      R.cases = R.c$R$`Mean(R)`, R.cases.q2.5 = R.c$R$`Quantile.0.025(R)`, R.cases.q97.5 = R.c$R$`Quantile.0.975(R)`)

write.csv(total.R,"../Data/Processed_Data/Rt/Rt_total.csv")


