
library(tidyverse)
library(ggplot2) 
library(readxl) 
library(lubridate)
library(broom)
library(viridis)
library(tsModel)
library(scales)
library(grid) 
library(tseries) 
library(tsibble) 
library(fable)
library(feasts) 
library(forecast)  
getwd()
source("./Scripts/functions.r")
# Just 2018 onward:   
monthly_f.ts <- prepare_ts("./CleanData/monthly_f.csv") 
glimpse(monthly_f.ts)
monthly_f.ts <- create_intervention(monthly_f.ts, "2021-06-01")

f_arima <- fit_arima(monthly_f.ts)

fc_f <- counterfactual_forecast(monthly_f.ts, "2021-06-01", 34)

plot_counterfactual(
  fc_f,
  monthly_f.ts,
  "2021-06-01",
  "Observed vs Counterfactual Female Assault Deaths"
)
monthly_f.ts <- prepare_ts("./CleanData/monthly_f.csv") 



#cleaning data: 
pre_18 <- read.csv("./RawData/Repro_Assault_Pre_2018/Female.csv")
pre_18 <- pre_18 %>%  
  filter(Month != "") %>% 
  mutate(month_year = my(Month),
         month = yearmonth(month_year),
         Deaths = na_if(as.character(Deaths), "Suppressed"),
         Deaths = as.numeric(Deaths),) %>% 
  select(-Notes,-Population,-"Crude.Rate", -Month, -"Month.Code") 

#we need to wait to make it a time series object until we've merged the data from before!! 
f_post_18 <- read.csv("./CleanData/monthly_f.csv") 
glimpse(f_post_18)
f_post_18 <- f_post_18 %>% 
  mutate(month_year = ymd(month_year),
         month = yearmonth(month_year)) %>% 
  select(-bill8:-time)
head(f_post_18)  
f_post_18 %>% 
  slice(15:20)

range(pre_18$month_year)
range(f_post_18$month_year)

female_full <- bind_rows(pre_18,f_post_18) 
female_full <- female_full %>% 
  mutate( time = row_number())
head(female_full)
tail(female_full) 
glimpse(female_full)
#correct number of observations 
#turning into ts 


f_ts <- female_full %>%
  as_tsibble(index = month)

#arima  
f_ts <- create_intervention(f_ts, "2021-06-01")

glimpse(f_ts) 

#before we fit arima, checking for gaps in time: 
# quick TRUE/FALSE
has_gaps(f_ts)

# show where gaps occur
scan_gaps(f_ts) 
 
#I rememer now! we subtracted the deaths from men to get the female total.
#that's why it's different. 

f_arima <- fit_arima(f_ts) 

report(f_arima)


fc_f <- counterfactual_forecast(f_ts, "2021-06-01", 40)

plot_counterfactual(
  fc_f,
  f_ts,
  "2021-06-01",
  "Observed vs Counterfactual Female Assault Deaths"
)
 


pre_period %>% 
  summarize(mean= mean(Deaths, na.rm=TRUE),
            sd= sd(Deaths, na.rm= TRUE))
