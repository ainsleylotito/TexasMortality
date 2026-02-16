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