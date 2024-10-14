## This document is to correct the case time series for each county in Ohio 
## Author: Julia Keating 03/25/2022

## load libraries
library(zoo)
library(dplyr)
library(roll)

series <- read.csv("../Data/Processed_Data/deaths_teaching_mobility.csv")
counties <- unique(series$county)

# we calculate 7-day rolling mean and median, and 14-day rolling sd (using IQR to estimate)
numbers <- data.frame(county=character(), date=character(), rmean_cases=numeric(), rmed_cases=numeric(), rsd_cases=numeric())
for (c in counties) {
  series.c <- filter(series, county==c) 
  vals <- data.frame(county = series.c[4:(nrow(series.c)-3),]$county, 
                     date = series.c[4:(nrow(series.c)-3),]$date, 
                     rmean_cases = rollmean(series.c$newconfirmed, k=7), 
                     rmed_cases = rollmedian(series.c$newconfirmed, k=7),
                     rsd_cases = rep(NA,nrow(series.c)-6))
  for (i in 8:(nrow(series.c)-7)) {
    vals[(i-3),]$rsd_cases <- IQR(series.c[i-7:i+7,]$newconfirmed) / ( 2*qnorm(3/4))
  }
  numbers <- rbind(numbers, vals)
}
series <- left_join(series, numbers, by=c("county", "date"))

# calculate upper and lower bound of "acceptance region" as median +/- 2.5 * sd
series$ylower <- ifelse(series$rmed_cases-2.5*series$rsd_cases<0, 0, series$rmed_cases-2.5*series$rsd_cases)
series$yupper <- series$rmed_cases+2.5*series$rsd_cases

# points outside "acceptance region" are flagged
series$flagged <- ifelse((is.na(series$ylower) | is.na(series$yupper)) | (series$newconfirmed>=series$ylower & series$newconfirmed<=series$yupper),0,1)

# we spread back the difference between the point and the rolling median 
series$tospreadback <- ifelse(series$flagged==0, 0, series$newconfirmed- series$rmed_cases)

series$baseline <- ifelse(series$flagged==0, series$newconfirmed, series$rmed_cases)
new_data <- data.frame(county=character(), date=character(), rev_newconfirmed2 = numeric())
for (c in counties) {
  series.c <- filter(series, county==c)
  newvals <- data.frame(county=series.c$county, 
                        date=series.c$date,
                        rev_newconfirmed2 = series.c$baseline)
  for (i in (1:nrow(series.c))) {
    if ((!is.na(series.c[i,]$flagged)) & ((series.c[i,]$flagged) == 1)) {
      lower = max(1,i-60)
      s = sum(series.c[lower:i,]$rmed_cases, na.rm=TRUE)
      y = ifelse(is.na(series.c[lower:i,]$rmed_cases), series.c[lower:i,]$newconfirmed, series.c[lower:i,]$rmed_cases)
      # weights are calculated using the rolling medians, and we go back no more than 60 days
      if (s==0) {
        weights = rep(0,i-lower+1) 
      }
      else {
        weights = y/s
        }
      toadd = c(rep(0,lower-1), weights*(series.c[i,]$tospreadback), rep(0,nrow(series.c)-i))
      newvals$rev_newconfirmed2 <- newvals$rev_newconfirmed2 + toadd
    }
  }
  new_data <- rbind(new_data, newvals)
}
series <- left_join(series, new_data, by=c("county", "date"))

series$flagged2 <- ifelse((is.na(series$ylower) | is.na(series$yupper)) | (series$rev_newconfirmed2>=series$ylower & series$rev_newconfirmed2<=series$yupper),0,1)

#sum(series$flagged, na.rm=TRUE)/nrow(series)
#sum(series$flagged2, na.rm=TRUE)/nrow(series)

series <- series[ , !names(series) %in% 
                    c("flagged","flagged2","tospreadback", "baseline", "X", "rmean_cases", "rsd_cases")]
series <- relocate(series, rev_newconfirmed2, .after=rev_newconfirmed) %>%
  relocate(rmed_cases, .after=rev_newconfirmed2) %>%
  relocate(ylower, .after=rmed_cases) %>%
  relocate(yupper, .after=ylower) 
  
write.csv(series,"../Data/Processed_Data/deaths_teaching_mobility.csv")
