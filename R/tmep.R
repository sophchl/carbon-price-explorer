
if (my_period == "5-year") {
  my_k = 5
  my_period = "yearly"
} else {
  my_k = 1
}

data_plot <- data_stock %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = my_period,
               k = my_k,
               type = mean_type) %>% 
  
  
  data_djia <- data_djia %>% 
  tq_transmute(select = c(date, open, high, low, close),
               mutate_fun = to.quarterly) %>% 
  mutate(return = (close - lag(close))/lag(close)) %>% 
  select(c("date", "return"))

data_gdp <- data_gdp %>% 
  mutate(date = as.yearqtr(date)) %>% 
  mutate(growth = (price - lag(price))/lag(price)) %>% 
  select(c("date", "growth")) 


data_djia <- data_djia %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "yearly",
               k = 5) %>% 
  mutate(year = year(date)) %>% 
  rename("return" = yearly.returns)

data_gdp <- data_gdp %>% 
  tq_transmute(select = price, 
               mutate_fun = periodReturn,
               period = "yearly", 
               k = 5) %>% 
  mutate(year = year(date)) %>% 
  rename("growth" = yearly.returns)


if(return_period == "daily"){
  data <- data %>% 
    select(c(date, adjusted)) %>% 
    drop_na()
}

if(return_period == "quarterly"){
  data <- data %>% 
    tq_transmute(select = adjusted,
                 mutate_fun = to.quarterly)
}

if (return_period == "5-year") {
  data <- data %>% 
    tq_transmute(select = adjusted,
                 mutate_fun = to.period,
                 period = "years",
                 k = 5)
}