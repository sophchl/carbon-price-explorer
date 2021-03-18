## explore carbon price and emission data

## carbon price scenarios from iias
carbon_price <- read.csv(file = "data/cd-links-carbon-price.csv") %>%
  slice(seq_len(nrow(carbon_price) - 1))

colnames(carbon_price) <- c(colnames(carbon_price)[0:5],
  substr(colnames(carbon_price[6:ncol(carbon_price)]), start = 2, stop = 5))

carbon_price %>%
  select(-Model, - Region, - Unit, -Variable) %>%
  gather(key = "year", value = "value", - Scenario) %>%
  arrange(Scenario) %>%
  mutate(year = as.double(year)) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(color = Scenario)) +
  labs(x = "year", y = "US$/tC02 per scenario") +
  theme_minimal()

## emission data per country for our world in data
ghg_data <- read_excel(path = "data/owid-co2-data.xlsx")

ghg_data_us <- ghg_data %>%
  filter(country == "United States" & year >= 1990) %>%
  select(c("year", "co2", "total_ghg"))

ghg_data_us %>%
  gather(key = "variable", value = "value", -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(color = variable)) +
  labs(x = "year", y = "Co2/GHG emissions") +
  scale_color_manual(values = c("darkred", "steelblue")) +
  theme_classic()
  
## emission data per sector from climatewatch
co2_data <- read.csv(file = "data/cw-co2-emissions.csv")
available_years_co2 <- colnames(co2_data)[3:ncol(co2_data)] %>%
  sapply(function(x, start, stop) substr(x, start, stop), start = 2, stop = 5)

plot(available_years_co2, co2_data[1, 3:ncol(co2_data)], type = "l",
  ylim = c(-500, 5800), ylab = "MtCo2 per sector", xlab = "year")
apply(co2_data[2:nrow(co2_data), 3:ncol(co2_data)], 1,
  function(x, t) lines(t, x), t = available_years_co2)

## data available:
# co2 emission sectors: energy, industrial, land-use
# djia sectors: Health, Financials, Consumer, Industrials,
# Information, Communication, Consumer Stables, Energy, Materials

## to do
# look at past climate events (policy) and what happened to dow jones
# - graphisch und returns berechnen
# look at sectors separately (e.g. green/brown)