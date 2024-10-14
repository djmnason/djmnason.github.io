
## This document is to compute the estimated reproduction number R for each county in Ohio 
## using both case and death time series
## Author: Julia Keating 03/4/2022
#setwd("C:/Users/Owner/CMU/Spring/PHIGHT_COVID")

## load libraries

library(ggh4x)
library(EpiEstim)
library(tidyverse)
library(kableExtra)

series.data <- read.csv("C:/Users/Owner/CMU/Spring/PHIGHT_COVID/deaths_teaching_mobility_posture.csv")
# map negative case numbers to 0, round decimals and convert to integers
series.data$rev_newconfirmed2 <- as.integer(round(ifelse(series.data$rev_newconfirmed2<0, 0, series.data$rev_newconfirmed2), digits=0))
series.data <- series.data %>%
  mutate(
    rev_newconfirmed2 = as.integer(round(ifelse(series.data$rev_newconfirmed2<0, 0, series.data$rev_newconfirmed2), digits=0)),
    date = as.Date(date),
    major_teaching_start = as.factor(major_teaching_start)
  )
## by county

new_data <- data.frame(county=character(), date=character(), 
                      R.deaths=numeric(), R.deaths.q2.5=numeric(), R.deaths.q97.5=numeric(),
                      R.cases=numeric(), R.cases.q2.5=numeric(), R.cases.q97.5=numeric())
for (c in unique(series.data$county)) {
  c.data <- series.data %>% 
    filter(county == c) %>% 
    select(c(2,12,14))
  
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


#write.csv(new_data,"../Data/Processed_Data/Rt/Rt_by_county.csv")

## slope scatter plot code

r_t_data <- left_join(
  series.data, new_data, by = c('date' = 'date', 'county' = 'county')) %>% 
  mutate(date = as.Date(date))

tmp_semester_data <- r_t_data %>%
  select(date, county, R.deaths, R.cases) %>% 
  filter(date %in% seq(as.Date('2020-10-01'), by = 'days', length.out = 42)) 

slope_df <- data.frame(county = character(),
                       Rt_cases_slope_first_3_weeks = numeric(),
                       Rt_cases_slope_next_3_weeks_cases = numeric(),
                       Rt_deaths_slope_first_3_weeks = numeric(),
                       Rt_deaths_slope_next_3_weeks = numeric())

for (c in unique(series.data$county)){
  # filter data
  tmp <- tmp_semester_data %>% filter(county == c & date %in% seq(as.Date('2020-10-01'), by = 'days', length.out = 21))
  # fit linear trend for weeks of interest and extract slope
  slope_0_3_case <- ((lm(R.cases ~ time(tmp %>% select(date) %>% pull()),
               data = tmp)) %>% coef())[[2]]
  slope_0_3_death <- ((lm(R.deaths ~ time(tmp %>% select(date) %>% pull()),
                         data = tmp)) %>% coef())[[2]]
  tmp <- tmp_semester_data %>% filter(county == c & date %in% seq(as.Date('2020-10-22'), by = 'days', length.out = 21))
  # fit linear trend for weeks of interest and extract slope
  slope_3_6_case <- ((lm(R.cases ~ time(tmp %>% select(date) %>% pull()),
                         data = tmp)) %>% coef())[[2]]
  slope_3_6_death <- ((lm(R.deaths ~ time(tmp %>% select(date) %>% pull()),
                          data = tmp)) %>% coef())[[2]]
  tmp_df <- data.frame(county = c,
                       Rt_cases_slope_first_3_weeks = slope_0_3_case,
                       Rt_cases_slope_next_3_weeks = slope_3_6_case,
                       Rt_deaths_slope_first_3_weeks = slope_0_3_death,
                       Rt_deaths_slope_next_3_weeks = slope_3_6_death)
  slope_df <- rbind(slope_df, tmp_df)
}

slope_df_plotter <- inner_join(slope_df,
          (r_t_data %>%
             select(
               county, population, population_density, metropolitan_status,
               nchs_urban_rural_status, major_teaching_start, major_teaching_end,
               posture_change)
           ) %>%
            distinct(),
          by = c('county' = 'county')) 

slope_df_plotter <- slope_df_plotter %>%
  inner_join(r_t_data %>%
               group_by(county) %>%
               summarise(mean_mobility = mean(full_work_prop_7d)
                         ),
             by = c('county' = 'county'))

addmargins(table(slope_df_plotter$nchs_urban_rural_status, slope_df_plotter$major_teaching_start)) %>%
  kbl(caption = "Summary statistics of word freq make", label = '')

# box plots
## no facets
slope_df_plotter %>%
  pivot_longer(
    cols = Rt_cases_slope_first_3_weeks:Rt_cases_slope_next_3_weeks,
    names_to = 'slope_type',
    values_to = 'slope') %>%
  ggplot(aes(x = major_teaching_start, y = slope, color = slope_type)) +
  geom_boxplot() 

slope_df_plotter %>%
  pivot_longer(
    cols = Rt_deaths_slope_first_3_weeks:Rt_deaths_slope_next_3_weeks_cases,
    names_to = 'slope_type',
    values_to = 'slope') %>%
  ggplot(aes(x = major_teaching_start, y = slope, color = slope_type)) +
  geom_boxplot()

# facets
# sub facets
slope_df_plotter %>%
  pivot_longer(
    cols = Rt_cases_slope_first_3_weeks:Rt_cases_slope_next_3_weeks,
    names_to = 'slope_type',
    values_to = 'slope') %>%
  ggplot(aes(x = major_teaching_start, y = slope, color = major_teaching_start)) +
  geom_boxplot() +
  facet_wrap(~slope_type)

slope_df_plotter %>%
  pivot_longer(
    cols = Rt_deaths_slope_first_3_weeks:Rt_deaths_slope_next_3_weeks,
    names_to = 'slope_type',
    values_to = 'slope') %>%
  ggplot(aes(x = major_teaching_start, y = slope, color = major_teaching_start)) +
  geom_boxplot() +
  facet_wrap(~slope_type)

slope_df_plotter %>%
  pivot_longer(
    cols = Rt_cases_slope_first_3_weeks:Rt_deaths_slope_next_3_weeks,
    names_to = 'slope_type',
    values_to = 'slope') %>%
  ggplot(aes(x = major_teaching_start, y = slope, color = major_teaching_start)) +
  geom_boxplot() +
  facet_wrap(~slope_type)

slope_df_plotter %>%
  pivot_longer(
    cols = Rt_cases_slope_first_3_weeks:Rt_cases_slope_next_3_weeks,
    names_to = 'slope_type',
    values_to = 'slope') %>%
  ggplot(aes(x = log(population_density), y = slope, color = major_teaching_start)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x, se = F) +
  facet_wrap(~slope_type) +
  theme_bw() +
  labs(
    title = 'Slope of Rt by county cases starting 10/1 against population density',
    y = 'Slope of Rt for cases by county')

slope_df_plotter %>%
  pivot_longer(cols = Rt_deaths_slope_first_3_weeks:Rt_deaths_slope_next_3_weeks, names_to = 'slope_type', values_to = 'slope') %>%
  ggplot(aes(x = log(population_density), y = slope, color = major_teaching_start)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x, se = F) +
  facet_wrap(~slope_type)+
  theme_bw() +
  labs(
    title = 'Slope of Rt by county deaths starting 10/1 against population density',
    y = 'Slope of Rt for deaths by county')


# by mobility
slope_df_plotter %>%
  pivot_longer(
    cols = Rt_cases_slope_first_3_weeks:Rt_cases_slope_next_3_weeks,
    names_to = 'slope_type',
    values_to = 'slope') %>%
  ggplot(aes(x = mean_mobility, y = slope, color = major_teaching_start)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x, se = F) +
  facet_wrap(~slope_type) +
  theme_bw() +
  labs(
    title = 'Slope of Rt by county cases starting 10/1 against mobility',
    y = 'Slope of Rt for cases by county')

slope_df_plotter %>%
  pivot_longer(cols = Rt_deaths_slope_first_3_weeks:Rt_deaths_slope_next_3_weeks, names_to = 'slope_type', values_to = 'slope') %>%
  ggplot(aes(x = mean_mobility, y = slope, color = major_teaching_start)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x, se = F) +
  facet_wrap(~slope_type)+
  theme_bw() +
  labs(
    title = 'Slope of Rt by county deaths starting 10/1 against mobility',
    y = 'Slope of Rt for deaths by county')

slope_df_plotter %>%
  pivot_longer(cols = Rt_cases_slope_first_3_weeks:Rt_deaths_slope_next_3_weeks, names_to = 'slope_type', values_to = 'slope') %>%
  ggplot(aes(x = mean_mobility, y = slope, color = major_teaching_start)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x, se = F) +
  facet_wrap(~slope_type)+
  theme_bw() +
  labs(
    title = 'Slope of Rt by county deaths starting 10/1 against mobility',
    y = 'Slope of Rt for deaths by county')


## density where point changes by mobility
slope_df_plotter %>%
  pivot_longer(
    cols = Rt_cases_slope_first_3_weeks:Rt_cases_slope_next_3_weeks,
    names_to = 'slope_type',
    values_to = 'slope') %>%
  ggplot(aes(x = log(population_density), y = slope, color = major_teaching_start)) +
  geom_point(aes(size = mean_mobility, alpha = mean_mobility)) +
  geom_smooth(method = 'lm', formula = y ~ x, se = F) +
  facet_wrap(~slope_type) +
  theme_bw() +
  labs(
    title = 'Slope of Rt by county cases starting 10/1 against population density',
    y = 'Slope of Rt for cases by county')

slope_df_plotter %>%
  pivot_longer(cols = Rt_deaths_slope_first_3_weeks:Rt_deaths_slope_next_3_weeks, names_to = 'slope_type', values_to = 'slope') %>%
  ggplot(aes(x = log(population_density), y = slope, color = major_teaching_start)) +
  geom_point(aes(size = mean_mobility, alpha = mean_mobility)) +
  geom_smooth(method = 'lm', formula = y ~ x, se = F) +
  facet_wrap(~slope_type)+
  theme_bw() +
  labs(
    title = 'Slope of Rt by county deaths starting 10/1 against population density',
    y = 'Slope of Rt for deaths by county')

### 
slope_df_plotter %>%
  pivot_longer(cols = Rt_deaths_slope_first_3_weeks:Rt_deaths_slope_next_3_weeks, names_to = 'slope_type', values_to = 'slope') %>%
  ggplot(aes(x = mean_mobility, y = slope, color = major_teaching_start)) +
  geom_point(aes(size = log(population_density), alpha = log(population_density))) +
  geom_smooth(method = 'lm', formula = y ~ x, se = F) +
  facet_wrap(~slope_type)+
  theme_bw() +
  labs(
    title = 'Slope of Rt by county deaths starting 10/1 against mobility',
    y = 'Slope of Rt for deaths by county')

slope_df_plotter %>%
  pivot_longer(cols = Rt_cases_slope_first_3_weeks:Rt_cases_slope_next_3_weeks, names_to = 'slope_type', values_to = 'slope') %>%
  ggplot(aes(x = mean_mobility, y = slope, color = major_teaching_start)) +
  geom_point(aes(size = log(population_density), alpha = log(population_density))) +
  geom_smooth(method = 'lm', formula = y ~ x, se = F) +
  facet_wrap(~slope_type)+
  theme_bw() +
  labs(
    title = 'Slope of Rt by county deaths starting 10/1 against mobility',
    y = 'Slope of Rt for deaths by county')



# narrow down on medium metro since it has more than 1 observation
slope_df_plotter %>%
  filter(nchs_urban_rural_status == 'Medium metro') %>%
  pivot_longer(cols = Rt_cases_slope_first_3_weeks:Rt_cases_slope_next_3_weeks_cases, names_to = 'slope_type', values_to = 'slope') %>%
  ggplot(aes(x = log(population_density), y = slope, color = major_teaching_start)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x, se = F) +
  facet_wrap(~slope_type) +
  theme_bw()

slope_df_plotter %>%
  filter(nchs_urban_rural_status == 'Medium metro') %>%
  pivot_longer(cols = Rt_deaths_slope_first_3_weeks:Rt_deaths_slope_next_3_weeks, names_to = 'slope_type', values_to = 'slope') %>%
  ggplot(aes(x = log(population_density), y = slope, color = major_teaching_start)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x, se = F) +
  facet_wrap(~slope_type)+
  theme_bw()

slope_df_plotter %>%
  pivot_longer(cols = Rt_cases_slope_first_3_weeks:Rt_deaths_slope_next_3_weeks, names_to = 'slope_type', values_to = 'slope') %>%
  ggplot(aes(x = log(population_density), y = slope, color = major_teaching_start)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x, se = F) +
  facet_wrap(~slope_type) +
  theme_bw()

slope_df_plotter %>%
  pivot_longer(cols = Rt_cases_slope_first_3_weeks:Rt_deaths_slope_next_3_weeks, names_to = 'slope_type', values_to = 'slope') %>%
  #filter(nchs_urban_rural_status == 'Micropolitan') %>%
  ggplot(aes(x = log(population_density), y = slope, color = major_teaching_start)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x, se = F) +
  facet_wrap(~slope_type, scales = 'free')

# slope_df_plotter %>%
#   pivot_longer(cols = Rt_cases_slope_first_3_weeks:Rt_deaths_slope_next_3_weeks, names_to = 'slope_type', values_to = 'slope') %>%
#   #filter(nchs_urban_rural_status == 'Micropolitan') %>%
#   ggplot(aes(x = log(population_density), y = slope, color = major_teaching_end)) +
#   geom_point() +
#   geom_smooth(method = 'lm', formula = y ~ x, se = F) +
#   facet_wrap(~slope_type, scales = 'free')

# slope_df_plotter %>%
#   pivot_longer(cols = Rt_cases_slope_first_3_weeks:Rt_deaths_slope_next_3_weeks, names_to = 'slope_type', values_to = 'slope') %>%
#   #filter(nchs_urban_rural_status == 'Micropolitan') %>%
#   ggplot(aes(x = log(population_density), y = slope, color = posture_change)) +
#   geom_point() +
#   geom_smooth(method = 'lm', formula = y ~ x, se = F) +
#   facet_wrap(~slope_type, scales = 'free')

slope_df_plotter %>%
  pivot_longer(cols = Rt_cases_slope_first_3_weeks:Rt_deaths_slope_next_3_weeks, names_to = 'slope_type', values_to = 'slope') %>%
  #filter(nchs_urban_rural_status == 'Micropolitan') %>%
  ggplot(aes(x = log(population_density), y = slope, color = nchs_urban_rural_status)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x, se = F) +
  facet_wrap(~slope_type, scales = 'free')

slope_df_plotter %>%
  pivot_longer(cols = Rt_cases_slope_first_3_weeks:Rt_deaths_slope_next_3_weeks, names_to = 'slope_type', values_to = 'slope') %>%
  #filter(nchs_urban_rural_status == 'Micropolitan') %>%
  ggplot(aes(x = log(population_density), y = slope, color = metropolitan_status)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x, se = F) +
  facet_wrap(~slope_type, scales = 'free')



# Transaction_Date %in% seq(as.Date(initial_date), as.Date(final_date), "days")


## by starting teaching posture

postures <- unique(series.data$major_teaching_start)

series.pos <- series.data %>%
  group_by(date, major_teaching_start) %>%
  summarize(
    rev_newdeaths=sum(rev_newdeaths),
    rev_newconfirmed2=sum(rev_newconfirmed2)
    )
 

new_data <- data.frame(major_teaching_start=factor(), date=character(),
                      R.deaths=numeric(), R.deaths.q2.5=numeric(), R.deaths.q97.5=numeric(),
                      R.cases=numeric(), R.cases.q2.5=numeric(), R.cases.q97.5=numeric())

for (p in levels(series.pos$major_teaching_start)) {
  p.data <- series.pos %>% 
    filter(postures == p) %>% 
    select(-major_teaching_start)
  
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
new_data <- new_data %>% mutate(major_teaching_start = as.factor(major_teaching_start))

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


