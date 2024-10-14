## This document is to create some of the plots associated with Rt
## Author: Julia Keating 04/16/2022

# load necessary libraries
library(ggplot2)
library(dplyr)
theme_set(theme_classic())

# read in data
series.data <- read.csv("../Data/Processed_Data/Rt/Rt_by_county.csv")
series.pop <- read.csv("../Data/Processed_data/deaths_teaching_mobility_posture.csv") 
  
series.data$date <- as.Date(series.data$date)

# plot Rt for the 9 largest counties (cases and deaths on the same plot)
largest_counties <- unique(series.pop[,c("county","population")]) %>%
  arrange(desc(population))
largest_counties <- largest_counties[1:9,]$county

cols <- c("Deaths"="blue", "Cases"="red")

ggplot(data=dplyr::filter(series.data, county %in% largest_counties & date>="2020-06-01"), aes(x=date, group=county)) +
  geom_rect(aes(xmin = as.Date("2020-08-15"), xmax = as.Date("2020-12-15"), ymin = -Inf, ymax = Inf), alpha = 0.1, fill = "grey95") +
  geom_ribbon(aes(ymin = R.deaths.q2.5, ymax = R.deaths.q97.5), fill = "blue", alpha=0.3) +
  geom_ribbon(aes(ymin = R.cases.q2.5, ymax = R.cases.q97.5), fill = "red", alpha=0.3) +
  geom_line(aes(y=R.deaths, colour="Deaths")) +
  geom_line(aes(y=R.cases, colour="Cases")) +
  labs(y="R_t", x="Date") +
  facet_wrap(~county, nrow=3) +
  scale_color_manual(name='Time Series', values=cols) 

# by teaching posture (cases and deaths on separate plots)
series.pos <- read.csv("../Data/Processed_Data/Rt/Rt_by_starting_posture.csv")
series.pos$date <- as.Date(series.pos$date)

ggplot(data=dplyr::filter(series.pos, date>="2020-06-01"), aes(x=date, group=major_teaching_start)) +
  geom_rect(aes(xmin = as.Date("2020-08-15"), xmax = as.Date("2020-12-15"), ymin = -Inf, ymax = Inf), alpha = 0.1, colour = "grey95", fill="grey95") +
  geom_vline(xintercept=as.Date("2020-09-15"), colour="dark grey", lty=2) +
  geom_vline(xintercept=as.Date("2020-10-06"), colour="dark grey", lty=2) +
  geom_vline(xintercept=as.Date("2020-10-27"), colour="dark grey", lty=2) +
  geom_ribbon(aes(ymin = R.deaths.q2.5, ymax = R.deaths.q97.5, fill=major_teaching_start), alpha=0.2) +
  geom_line(aes(y=R.deaths, colour=major_teaching_start)) +
  labs(y=TeX("$R_t$ from deaths"), x="Date", fill="Majority starting teaching posture", colour="Majority starting teaching posture")

ggplot(data=dplyr::filter(series.pos, date>="2020-06-01"), aes(x=date, group=major_teaching_start)) +
  geom_rect(aes(xmin = as.Date("2020-08-15"), xmax = as.Date("2020-12-15"), ymin = -Inf, ymax = Inf), alpha = 0.1, colour = "grey95", fill="grey95") +
  geom_ribbon(aes(ymin = R.cases.q2.5, ymax = R.cases.q97.5, fill=major_teaching_start), alpha=0.3) +
  geom_line(aes(y=R.cases, colour=major_teaching_start)) +
  labs(y="R_t from Cases", x="Date")

## by change in teaching posture (cases and deaths on separate plots)
series.pos <- read.csv("../Data/Processed_Data/Rt/Rt_by_posture_change.csv")
series.pos$date <- as.Date(series.pos$date)

ggplot(data=dplyr::filter(series.pos, date>="2020-06-01"), aes(x=date, group=posture_change)) +
  geom_rect(aes(xmin = as.Date("2020-08-15"), xmax = as.Date("2020-12-15"), ymin = -Inf, ymax = Inf), alpha = 0.1, colour = "grey95", fill="grey95") +
  geom_ribbon(aes(ymin = R.deaths.q2.5, ymax = R.deaths.q97.5, fill=posture_change), alpha=0.3) +
  geom_line(aes(y=R.deaths, colour=posture_change)) +
  labs(y="R_t", x="Date") +
  ggtitle("R_t estimates using time series of deaths") 

ggplot(data=dplyr::filter(series.pos, date>="2020-06-01"), aes(x=date, group=posture_change)) +
  geom_rect(aes(xmin = as.Date("2020-08-15"), xmax = as.Date("2020-12-15"), ymin = -Inf, ymax = Inf), alpha = 0.1, colour = "grey95", fill="grey95") +
  geom_ribbon(aes(ymin = R.cases.q2.5, ymax = R.cases.q97.5, fill=posture_change), alpha=0.3) +
  geom_line(aes(y=R.cases, colour=posture_change)) +
  labs(y="R_t from Cases", x="Date")

## by teaching posture for micro counties (cases and deaths on separate plots)
series.pos <- read.csv("../Data/Processed_Data/Rt/Rt_by_starting_posture_micro.csv")
series.pos$date <- as.Date(series.pos$date)

ggplot(data=dplyr::filter(series.pos, date>="2020-06-01"), aes(x=date, group=major_teaching_start)) +
  geom_rect(aes(xmin = as.Date("2020-08-15"), xmax = as.Date("2020-12-15"), ymin = -Inf, ymax = Inf), alpha = 0.1, colour = "grey95", fill="grey95") +
  geom_ribbon(aes(ymin = R.deaths.q2.5, ymax = R.deaths.q97.5, fill=major_teaching_start), alpha=0.3) +
  geom_line(aes(y=R.deaths, colour=major_teaching_start)) +
  labs(y="R_t", x="Date") +
  ggtitle("R_t estimates using time series of deaths for only Micropolitan Counties") 

ggplot(data=dplyr::filter(series.pos, date>="2020-06-01"), aes(x=date, group=major_teaching_start)) +
  geom_rect(aes(xmin = as.Date("2020-08-15"), xmax = as.Date("2020-12-15"), ymin = -Inf, ymax = Inf), alpha = 0.1, colour = "grey95", fill="grey95") +
  geom_ribbon(aes(ymin = R.cases.q2.5, ymax = R.cases.q97.5, fill=major_teaching_start), alpha=0.3) +
  geom_line(aes(y=R.cases, colour=major_teaching_start)) +
  labs(y="R_t", x="Date") +
  ggtitle("R_t estimates using time series of cases for only Micropolitan Counties")

## by urban rural status (cases and deaths on separate plots)
series.urs <- read.csv("../Data/Processed_Data/Rt/Rt_by_nchs_urban_rural.csv")
series.urs$date <- as.Date(series.urs$date)

ggplot(data=dplyr::filter(series.urs, date>="2020-06-01"), aes(x=date, group=nchs_urban_rural_status)) +
  geom_rect(aes(xmin = as.Date("2020-08-15"), xmax = as.Date("2020-12-15"), ymin = -Inf, ymax = Inf), alpha = 0.1, colour = "grey95", fill="grey95") +
  geom_ribbon(aes(ymin = R.deaths.q2.5, ymax = R.deaths.q97.5, fill=nchs_urban_rural_status), alpha=0.3) +
  geom_line(aes(y=R.deaths, colour=nchs_urban_rural_status)) +
  labs(y="R_t from Deaths", x="Date")

ggplot(data=dplyr::filter(series.urs, date>="2020-06-01"), aes(x=date, group=nchs_urban_rural_status)) +
  geom_rect(aes(xmin = as.Date("2020-08-15"), xmax = as.Date("2020-12-15"), ymin = -Inf, ymax = Inf), alpha = 0.1, colour = "grey95", fill="grey95") +
  geom_ribbon(aes(ymin = R.cases.q2.5, ymax = R.cases.q97.5, fill=nchs_urban_rural_status), alpha=0.3) +
  geom_line(aes(y=R.cases, colour=nchs_urban_rural_status)) +
  labs(y="R_t from Cases", x="Date")

## by metropolitan status (cases and deaths on separate plots)
series.ms <- read.csv("../Data/Processed_Data/Rt/Rt_by_metropolitan.csv")
series.ms$date <- as.Date(series.ms$date)

ggplot(data=dplyr::filter(series.ms, date>="2020-06-01"), aes(x=date, group=metropolitan_status)) +
  geom_rect(aes(xmin = as.Date("2020-08-15"), xmax = as.Date("2020-12-15"), ymin = -Inf, ymax = Inf), alpha = 0.1, colour = "grey95", fill="grey95") +
  geom_ribbon(aes(ymin = R.deaths.q2.5, ymax = R.deaths.q97.5, fill=metropolitan_status), alpha=0.3) +
  geom_line(aes(y=R.deaths, colour=metropolitan_status)) +
  labs(y="R_t from Deaths", x="Date")

ggplot(data=dplyr::filter(series.ms, date>="2020-06-01"), aes(x=date, group=metropolitan_status)) +
  geom_rect(aes(xmin = as.Date("2020-08-15"), xmax = as.Date("2020-12-15"), ymin = -Inf, ymax = Inf), alpha = 0.1, colour = "grey95", fill="grey95") +
  geom_ribbon(aes(ymin = R.cases.q2.5, ymax = R.cases.q97.5, fill=metropolitan_status), alpha=0.3) +
  geom_line(aes(y=R.cases, colour=metropolitan_status)) +
  labs(y="R_t from Cases", x="Date")

## total (cases and deaths on same plot)
series.total <- read.csv("../Data/Processed_Data/Rt/Rt_total.csv")
series.total$date <- as.Date(series.total$date)

cols <- c("Deaths"="black", "Cases"="red")

ggplot(data=dplyr::filter(series.total, date>="2020-06-01"), aes(x=date)) +
  geom_rect(aes(xmin = as.Date("2020-08-15"), xmax = as.Date("2020-12-15"), ymin = -Inf, ymax = Inf), alpha = 0.1, fill = "grey95") +
  geom_ribbon(aes(ymin = R.deaths.q2.5, ymax = R.deaths.q97.5), fill = "black", alpha=0.3) +
  geom_ribbon(aes(ymin = R.cases.q2.5, ymax = R.cases.q97.5), fill = "red", alpha=0.3) +
  geom_line(aes(y=R.deaths, colour="Deaths")) +
  geom_line(aes(y=R.cases, colour="Cases")) +
  labs(y=TeX("$R_t$"), x="Date") +
  scale_color_manual(name='Time Series', values=cols) 


## 3 largest counties in each teaching posture (cases and deaths on same plot):
hybrid = unique(dplyr::filter(series.pop, major_teaching_start=="Hybrid")[,c("county","population")]) %>%
  arrange(desc(population))
hybrid <- hybrid[1:3,]$county

on_premises = unique(dplyr::filter(series.pop, major_teaching_start=="On_Premises")[,c("county","population")]) %>%
  arrange(desc(population))
on_premises <- on_premises[1:3,]$county

online_only = unique(dplyr::filter(series.pop, major_teaching_start=="Online_Only")[,c("county","population")]) %>%
  arrange(desc(population))
online_only <- online_only[1:3,]$county

p1 <- ggplot(data=dplyr::filter(series.data, county %in% hybrid & date>="2020-06-01"), aes(x=date, group=county)) +
  geom_rect(aes(xmin = as.Date("2020-08-15"), xmax = as.Date("2020-12-15"), ymin = -Inf, ymax = Inf), alpha = 0.1, colour = "grey95", fill="grey95") +
  geom_ribbon(aes(ymin = R.deaths.q2.5, ymax = R.deaths.q97.5), fill="blue", alpha=0.3) +
  geom_ribbon(aes(ymin = R.cases.q2.5, ymax = R.cases.q97.5), fill = "red", alpha=0.3) +
  geom_line(aes(y=R.deaths, colour="Deaths")) +
  geom_line(aes(y=R.cases, colour="Cases")) +
  labs(y="R_t", x="Date") +
  facet_grid("Hybrid" ~ county, scales="free_y") +
  scale_color_manual(name='Time Series', values=cols) 

p2 <- ggplot(data=dplyr::filter(series.data, county %in% on_premises & date>="2020-06-01"), aes(x=date, group=county)) +
  geom_rect(aes(xmin = as.Date("2020-08-15"), xmax = as.Date("2020-12-15"), ymin = -Inf, ymax = Inf), alpha = 0.1, colour = "grey95", fill="grey95") +
  geom_ribbon(aes(ymin = R.deaths.q2.5, ymax = R.deaths.q97.5), fill="blue", alpha=0.3) +
  geom_ribbon(aes(ymin = R.cases.q2.5, ymax = R.cases.q97.5), fill = "red", alpha=0.3) +
  geom_line(aes(y=R.deaths, colour="Deaths")) +
  geom_line(aes(y=R.cases, colour="Cases")) +
  labs(y="R_t", x="Date") +
  facet_grid("On premises" ~ county, scales="free_y") +
  scale_color_manual(name='Time Series', values=cols) 

p3 <- ggplot(data=dplyr::filter(series.data, county %in% online_only & date>="2020-06-01"), aes(x=date, group=county)) +
  geom_rect(aes(xmin = as.Date("2020-08-15"), xmax = as.Date("2020-12-15"), ymin = -Inf, ymax = Inf), alpha = 0.1, colour = "grey95", fill="grey95") +
  geom_ribbon(aes(ymin = R.deaths.q2.5, ymax = R.deaths.q97.5), fill="blue", alpha=0.3) +
  geom_ribbon(aes(ymin = R.cases.q2.5, ymax = R.cases.q97.5), fill = "red", alpha=0.3) +
  geom_line(aes(y=R.deaths, colour="Deaths")) +
  geom_line(aes(y=R.cases, colour="Cases")) +
  labs(y="R_t", x="Date") +
  facet_grid("Online only" ~ county, scales="free_y") +
  scale_color_manual(name='Time Series', values=cols) 

cowplot::plot_grid(p1, p2, p3, nrow = 3, align = 'v', axis = 'lr')

## plot Rt against mobility (deaths and cases seperately)
mobility <- unique(series.pop[,c("county","major_teaching_start", "full_work_prop_7d", "date")])

series.mob <- merge(series.data, mobility, by=c("county", "date"))

ggplot(data=dplyr::filter(series.mob, date>="2020-10-22" & date<="2020-11-11"), 
       aes(x=full_work_prop_7d, y=log(R.cases), group=major_teaching_start)) +
  geom_point(aes(colour=major_teaching_start), cex=0.5) +
  labs(x="mobility", y="log(Rt)") +
  ggtitle("log(Rt) using case time series vs mobility")

ggplot(data=dplyr::filter(series.mob, date>="2020-10-22" & date<="2020-11-11"), 
       aes(x=full_work_prop_7d, y=log(R.deaths), group=major_teaching_start)) +
  geom_point(aes(colour=major_teaching_start), cex=0.5) +
  labs(x="mobility", y="log(Rt)") + 
  ggtitle("log(Rt) using death time series vs mobility")

## si distributions for cases and deaths
si_distr.d=dgamma(0:100, shape=(1/0.4)^2, rate=1/(23.9*(0.4^2)))
si_distr.c=dgamma(0:100, shape=(1/0.6)^2, rate=1/(15.2*(0.6^2)))

gammas <- data.frame(index = c(0:100), deaths = si_distr.d, cases = si_distr.c)

ggplot(data=gammas, aes(x=index)) +
  geom_bar(aes(y=deaths, fill='lightskyblue'), color='lightskyblue4', stat="identity", position ="identity", alpha=.5) +
  geom_bar(aes(y=cases, fill="palegreen"), color="palegreen4", stat="identity", position="identity", alpha=.3) +
  labs(y="Probability") +
  scale_fill_manual(values=c("lightskyblue", "palegreen"), name="Time series", labels=c("Deaths", "Cases"))