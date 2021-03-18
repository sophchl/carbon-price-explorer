## carbon price scenarios

carbon_price <- read.csv(file = "data/cd-links-carbon-price.csv")

available_years_price = colnames(carbon_price)[6:ncol(carbon_price)] %>% 
  sapply(function(x,start,stop) substr(x,start,stop), start=2, stop = 5)

plot(available_years_price, carbon_price[1,6:ncol(carbon_price)], type = "l", ylim = c(-5,1400), ylab = "US$/tC02 per scenario", xlab = "year")
apply(carbon_price[2:nrow(carbon_price),6:ncol(carbon_price)], 1, function(x,t) lines(t,x), t = available_years_price)

## emission data per country
ghg_data <- read_excel(path = "data/owid-co2-data.xlsx")

ghg_data_us <- ghg_data %>% 
  filter(country == "United States" & year >= 1990) %>% 
  select(c("country", "year", "co2", "total_ghg"))

## emission data per sector

co2_data <- read.csv(file = "data/cw-co2-emissions.csv")
available_years_co2 = colnames(co2_data)[3:ncol(co2_data)] %>% 
  sapply(function(x,start,stop) substr(x,start,stop), start=2, stop = 5)

plot(available_years_co2, co2_data[1,3:ncol(co2_data)], type = "l", ylim = c(-500, 5800), ylab = "MtCo2 per sector", xlab = "year")
apply(co2_data[2:nrow(co2_data),3:ncol(co2_data)], 1, function(x,t) lines(t,x), t = available_years_co2)

## data available:
# co2 emission sectors: energy, industrial, land-use
# djia sectors: Health, Financials, Consumer, Industrials, Information, Communication, Consumer Stables, Energy, Materials