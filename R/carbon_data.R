### explore carbon price and emission data

## carbon price scenarios for iias
## emission data per country and per industry in the US

#--------------------------------------------------------------------

## carbon price scenarios from iias: how much will carbon cost in each scenario?
carbon_price <- read.csv(file = "data/cd-links-carbon-price.csv")
carbon_price <- carbon_price %>% slice(seq_len(nrow(carbon_price) - 1))

# rename columns to incorporate only year
colnames(carbon_price) <- c(colnames(carbon_price)[0:5],
  substr(colnames(carbon_price[6:ncol(carbon_price)]), start = 2, stop = 5))

# plot
carbon_price %>%
  select(-Model, - Region, - Unit, -Variable) %>%
  gather(key = "year", value = "value", - Scenario) %>%
  arrange(Scenario) %>%
  mutate(year = as.double(year)) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(color = Scenario)) +
  labs(x = "year", y = "US$/tC02 per scenario") +
  theme_classic()

## emission data per country for our world in data
ghg_data <- read_excel(path = "data/owid-co2-data.xlsx")

# select only US and co2 data (total ghg - all ghg in co2 equivalents)
ghg_data_us <- ghg_data %>%
  filter(country == "United States" & year >= 1990) %>%
  select(c("year", "co2", "total_ghg"))

# plot
ghg_data_us %>%
  gather(key = "variable", value = "value", -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(color = variable)) +
  labs(x = "year", y = "Co2/GHG emissions") +
  scale_color_manual(values = c("darkred", "steelblue")) +
  theme_classic()
  
## emission data per sector from climatewatch
ghg_sector_data <- read.csv(file = "data/cw-co2-emissions.csv")

# rename columns to incorporate only year
colnames(ghg_sector_data) <- c(colnames(ghg_sector_data)[1:2],
  substr(colnames(ghg_sector_data[- (1:2)]), start = 2, stop = 5))

ghg_sector_data  %>%
  select(-unit)  %>%
  slice(1:(n() - 2)) %>%
  gather(key = "year", value = "value", - Sector)  %>%
  na_if("false")  %>%
  drop_na()  %>%
  mutate(year = as.double(year), value = as.double(value)) %>%
  arrange(Sector)  %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(color = Sector)) +
  theme_classic()

## data available:
# co2 emission sectors: energy, industrial, land-use
# djia sectors: Health, Financials, Consumer, Industrials,
# Information, Communication, Consumer Stables, Energy, Materials

## to do
# look at past climate events (policy) and what happened to dow jones
# - graphisch und returns berechnen
# look at sectors separately (e.g. green/brown)