djia_stocks_raw = tq_index("DOW") %>% 
  tq_get(get = "stock.prices", from = "2020-01-01")

djia_stocks <- djia_stocks_raw %>%
  group_by(symbol) %>% 
  tq_mutate(adjusted, mutate_fun = dailyReturn) %>% 
  select(c("symbol", "company", "weight", "sector", "date", "adjusted", "daily.returns")) %>% 
  ungroup()

all_sectors <- unique(djia_stocks$sector)




