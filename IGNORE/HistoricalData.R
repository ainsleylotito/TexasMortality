
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
f_post_18 <- read_excel("./RawData/MonthlyAssaults.xlsx") 
f_post_18 <- f_post_18 %>% 
  mutate(month_year = my(Month),
         month = yearmonth(month_year)) %>% 
  select(-Notes,-Population,-"Crude Rate", -Month, -"Month Code") %>% 
  filter(month_year < "2024-01-01")
head(f_post_18) 

range(pre_18$month_year)
range(f_post_18$month_year)

female_full <- bind_rows(pre_18,f_post_18) 
female_full <- female_full %>% 
  mutate( time = row_number())
head(female_full)
tail(female_full) 
#correct number of observations 
#turning into ts 

unique(female_full$month_year) 


f_ts <- female_full %>%
  as_tsibble(index = month)

#arima  
f_ts <- create_intervention(f_ts, "2021-06-01")

intervention_time <- f_ts %>%
  filter(month_year == as.Date("2021-06-01")) %>%
  pull(time)

f_ts <- f_ts %>%
  mutate(
    t = time,
    step = if_else(t >= intervention_time, 1, 0),
    ramp = if_else(t >= intervention_time, t - intervention_time + 1, 0)
  ) 


f_arima <- fit_arima(f_ts) 

fc_f <- counterfactual_forecast(f_ts, "2021-06-01", 34)

f_arima <- f_ts %>%
  model(ARIMA(Deaths ~ step + ramp,
              stepwise = FALSE))

report(f_arima)



fc_f <- counterfactual_forecast(f_ts, "2021-06-01", 34)

plot_counterfactual(
  fc_f,
  f_ts,
  "2021-06-01",
  "Observed vs Counterfactual Female Assault Deaths"
)
