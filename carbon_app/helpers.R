djia <- tq_get("DJIA", get = "stock.prices", from = "1990-01-01")
gdp <- tq_get("GDP", get = "economic.data", from = "1990-01-01")
scenarios <- read.csv(file = "data/cd-links-gdp-us.csv")

#scenarios <- read.csv(file = "carbon_app/data/cd-links-gdp-us.csv")

# prepare djia data

djia_prepare_daily <- function(data, mean_type)  {
  # prepares djia data for daily analysis
  # input: djia as loaded by tidyquant, mean_type: "arithmetic" or "log"
  # output: djia with: date (ymd), open, high, low, close, volume, adjusted, daily.return
  
  data_out <- data %>%
    tq_mutate(select = adjusted,
              mutate_fun = periodReturn,
              period = "daily",
              type = mean_type)  %>%
    mutate(date = ymd(date))  %>%
    select(-symbol)
  
  return(data_out)
  
}

djia_prepare_quarterly <- function(data, mean_type)  {
  # prepares djia data for daily analysis
  # input: djia as loaded by tidyquant, mean_type: "arithmetic" or "log"
  # output: djia with: date (ymd), open, high, low, close, volume, adjusted, daily.return
  
  data_out <- djia %>% 
    mutate(date = ymd(date)) %>% 
    tq_transmute(select = c(date, open, high, low, close, volume, adjusted),
                 mutate_fun = to.quarterly
    ) %>% 
    tq_mutate(select = adjusted,
              mutate_fun = periodReturn,
              period = "quarterly",
              type = mean_type)
  
  return(data_out)
  
}

# calculate a function that calculates volatility for selected days

my_vola_fun <- function(data, vector_days) {
  # calculates volatility for a vector of days (loop so not super fast)
  # input: data, vector that holds the aggregation periods to calculate volatility for
  # output: tibble with period and vola
  
  vola_vector <- c()
  for (i in seq_along(vector_days)){
    return_vola <- volatility(data[, c("open", "high", "low", "close")],
                              n = vector_days[i], calc = "close") %>%
      mean(na.rm = T)
    
    vola_vector[i] <- return_vola
  }
  vola_data <- data %>% summarise(period = vector_days,
                                  djia_vola = vola_vector)
  
  return(vola_data)
}

my_vola_fun_progress <- function(data, vector_days) {
  # calculates volatility for a vector of days (loop so not super fast)
  # input: data, vector that holds the aggregation periods to calculate volatility for
  # output: tibble with period and vola
  
  vola_vector <- c()
  
  withProgress(message = "Making plot", value = 0, {
    
    # number of times we go through the loop
    n <- vector_days[length(vector_days)] - vector_days[1] + 1
    
    # go through the loop
    for (i in seq_along(vector_days)){
      
      # add data in every step
      return_vola <- volatility(data[, c("open", "high", "low", "close")],
                                n = vector_days[i], calc = "close") %>%
        mean(na.rm = T)
      
      vola_vector[i] <- return_vola
      
      # update the progress bar
      incProgress(1/n, detail = paste("Doing part", i))
    }
    
    
  })
  
  vola_data <- data %>% summarise(period = vector_days,
                                  djia_vola = vola_vector)
  
  return(vola_data)
}

# prepare scenario data
scenario_prepare <- function(data) {
  
  colnames(data) <- c(colnames(data)[0:5],
                           substr(colnames(data[6:ncol(data)]), start = 2, stop = 5))
  data <- data %>% slice(seq_len(nrow(data) - 1))
  
  return(data)
}


# plot for djia historic volatility (daily)

plot_djia_volatility_daily <- function(data, mean_type, time_frame) {
  # creates a plot of volatility against aggregation period
  # input: djia raw data, mean_type, time_frame
  # output: ggplot
  
  data_plot <- data %>% 
    djia_prepare_daily(mean_type) %>% 
    my_vola_fun_progress(time_frame)
  
  ggplot(data_plot, aes(x = period, fill = djia_vola, y = djia_vola)) +
    geom_area(fill = "#8DD3C7", color = "#1B9E77") +
    #geom_line() +
    xlab("aggregation horizon") + 
    ylab("DJIA volatility") +
    coord_cartesian(ylim = c(min(data_plot$djia_vola), max(data_plot$djia_vola)),
                    xlim = c(time_frame[1], tail(time_frame,1))) +
    theme_classic()
  
    #xlim(time_frame[1], tail(time_frame,1)) +
    #ylim(min(data_plot$djia_vola) %>% round(2), max(data_plot$djia_vola) %>% round(2)) +
}

# descriptive plots
plot_descriptives_quarterly <- function(data_stock, mean_type, data_gdp) {
  
  data_djia <- djia_prepare_quarterly(data_stock, mean_type) %>% 
    select(date, adjusted)
  
  data_gdp <- gdp %>% tq_transmute(mutate_fun = to.quarterly)
  
  data_plot <- left_join(data_gdp, data_djia) %>% 
    rename("GDP in billion dollar"  = price, "DJIA price" = adjusted) %>% 
    gather(key = "variable", value = "value", -date)
                           
  ggplot(data = data_plot, aes(x = date, y = value, color = variable)) +
    geom_line() +
    theme_classic() +
    scale_color_manual(values = c("#1B9E77", "#7570B3"))
  
}

plot_descriptives_return <- function(data_stock, mean_type) {
  
  data_djia <- djia_prepare_daily(data_stock, mean_type)
  
  ggplot(data_djia, aes(x = date, y = daily.returns)) +
    geom_line(color = "8c8c8c") + #"#1B9E77"
    ylab("DJIA return") +
    theme_classic()
  
}

plot_descriptives_gdp <- function(data_historic, data_scenario) {
  
  data_scenario <- data_scenario %>%
    scenario_prepare() %>% 
    select(-Model, - Region, - Unit, -Variable) %>%
    gather(key = "date", value = "value", - Scenario) %>% 
    mutate(date = ymd(date, truncated = 2L))
  
  data_historic <- data_historic %>% 
    rename(historic = price) %>% 
    select(-symbol) %>% 
    gather(key = "Scenario", value = "value", -date)
  
  data_plot <- rbind(data_scenario, data_historic)
  
  data_plot %>% 
    ggplot(aes(x = date, y = value)) +
    geom_line(aes(color = Scenario)) +
    labs(x = "year", y = "GDP in billion dollar") +
    theme_classic() 
  
}

plot_correlation <- function(data_stock, mean_type, data_gdp) {
  
}




