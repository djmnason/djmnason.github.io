## This document is to combine all processed tables
## and combine it with Ohio county profile data from CDC
## Author: Ziyan Zhu 
## Updated on 03/04/2022 by Julia Keating:
##      1. updated variable names on line 16, 18, 28, 29
##      2. updated file path on line 35

### loading processor
source("../Data_processor/002_OHK12_data_processor.R")
source("../Data_processor/003_mobility_collecter.R")
source("../Data_processor/004_Ohio_case_profile_data_processor.R")

################# County-level mobility & case data ################
## calculate death per 1000 people
deaths_teaching <- cases%>%
  left_join(enroll_by_teaching_wide,by=c('county'='county'))%>%
  drop_na(major_teaching)%>%
  mutate(death_prop = 1000*round(cumdeaths/population,5))%>%
  mutate(death_per_1000 = death_prop)

deaths_teaching$major_teaching <- factor(deaths_teaching$major_teaching,
                                                    levels = c("On Premises","Hybrid","Online Only"))


# SHIFT the DATE for mobility: mobility a week ago may impact the infections number now
deaths_teaching_mobility <- deaths_teaching %>%
      inner_join((mobility%>%
                  rename(fips = geo_value,date = time_value)%>%
                  mutate(date = date -7)),by=c("fips","date"))

deaths_teaching_mobility$major_teaching <- factor(deaths_teaching_mobility$major_teaching,
                                            levels = c("On Premises","Hybrid","Online Only"))


write.csv(deaths_teaching_mobility,"../Data/Processed_Data/deaths_teaching_mobility.csv")

