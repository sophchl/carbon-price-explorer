carbon_data <- read.csv(file = "data/cd-links-carbon-price.csv")

available_years = colnames(carbon_data)[6:ncol(carbon_data)] %>% 
  sapply(function(x,start,stop) substr(x,start,stop), start=2, stop = 5)

plot(available_years, carbon_data[1,6:ncol(carbon_data)], type = "l", ylim = c(-5,1400), ylab = "US$/tC02", xlab = "year")
apply(carbon_data[2:nrow(carbon_data),6:ncol(carbon_data)], 1, function(x,t) lines(t,x), t = available_years)

