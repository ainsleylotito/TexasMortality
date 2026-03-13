# cleaning .csv function 

clean.csv <- function(df){
  df %>% 
  mutate(month_year = my(Month)) %>% 
  select(-Notes,-Population,-"Crude.Rate", -Month, -"Month.Code") %>% 
  filter(month_year < "2024-01-01") %>% 
  mutate(bill8 = month_year > "2021-08-01", 
         time = row_number ())
}


#cleaning xcel: 
clean.xcel <- function(df){
  df %>%  
    mutate(month_year = my(Month)) %>% 
    select(-Notes,-Population,-"Crude Rate", -Month, -"Month Code") %>% 
    filter(month_year < "2024-01-01") %>% 
    mutate(bill8 = month_year > "2021-08-01", 
           time = row_number ())
}

prepare_ts <- function(file){
  
  read.csv(file) %>%
    mutate(
      month_year = ymd(month_year),
      month = yearmonth(month_year)
    ) %>%
    as_tsibble(index = month)
  
} 

#cleaning racial/ethnic 

clean_racial <- function(nh, h){
  
  nh <- nh %>% 
    filter(Single.Race.6 != "",
           !is.na(Month)) %>% 
    mutate(
      month_year = my(Month),
      Deaths = na_if(Deaths, 0)
    ) %>%
    select(-Notes,-Month, -Month.Code, -Single.Race.6.Code,
           -Population:-Crude.Rate.Upper.95..Confidence.Interval) %>%
    rename(race_eth = Single.Race.6)
  
  h <- h %>% 
    filter(!is.na(Month)) %>% 
    mutate(
      month_year = my(Month),
      race_eth = "Hispanic",
      Deaths = na_if(Deaths, 0)
    ) %>%
    select(-Notes:-Month.Code, 
           -Population:-Crude.Rate.Upper.95..Confidence.Interval)
  
  racial <- bind_rows(nh, h) %>%
    arrange(race_eth, month_year)
  
  return(racial)
}


create_intervention <- function(ts_data, intervention_date){
  
  intervention_time <- ts_data %>%
    filter(month_year == intervention_date) %>%
    pull(time)
  
  ts_data %>%
    mutate(
      t = row_number(),
      step = if_else(t >= intervention_time, 1, 0),
      ramp = if_else(t >= intervention_time,
                     t - intervention_time + 1,
                     0)
    )
}

fit_arima <- function(ts_data){
  
  ts_data %>%
    model(
      arima = ARIMA(
        Deaths ~ step + ramp,
        stepwise = FALSE
      )
    )
  
}

counterfactual_forecast <- function(ts_data, intervention_date, horizon){
  
  model_cf <- ts_data %>%
    filter(month < make_yearmonth(year(intervention_date),
                                  month(intervention_date))) %>%
    model(
      arima_null = ARIMA(Deaths, stepwise = FALSE)
    )
  
  forecast(model_cf, h = horizon)
  
}

plot_counterfactual <- function(fc, ts_data, intervention_date, title){
  
  autoplot(fc, ts_data, level = 95) +
    geom_vline(
      xintercept = as.numeric(as_date(make_yearmonth(year(intervention_date),
                                                     month(intervention_date)))),
      linetype = "dashed",
      color = "grey40"
    ) +
    labs(
      title = title,
      y = "Monthly Deaths",
      x = "Month"
    ) +
    theme_minimal()
  
}