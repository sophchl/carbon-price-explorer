# scenarios <- read.csv(file = "carbon_app/data/cd-links-gdp-us.csv")
# app_diretory: 'C:/Users/Sophia/Dropbox/01_Studium/04_projects/CarbonPriceExplorer/carbon_app'

## data transformations ------------------

# prepare djia data

djia_prepare_daily <- function(data, mean_type)  {
  # prepares djia data for daily analysis
  # input: data: djia as loaded by tidyquant, mean_type: "arithmetic" or "log"
  # output: djia with columns date (ymd), open, high, low, close, volume, adjusted, daily.return
  
  data_out <- data %>%
    tq_mutate(select = adjusted,
              mutate_fun = periodReturn,
              period = "daily",
              type = mean_type)  %>%
    mutate(date = ymd(date))  %>%
    select(-symbol)
  
  return(data_out)
  
}

# prepare scenario data

scenario_prepare <- function(data) {
  # remove strings from column names and remove last row with source information
  # input: data: scenario data from iiasa
  
  colnames(data) <- c(colnames(data)[0:5],
                      substr(colnames(data[6:ncol(data)]), start = 2, stop = 5))
  data <- data %>% slice(seq_len(nrow(data) - 1))
  
  return(data)
}

# calculate a function that calculates volatility for selected days

my_vola_fun <- function(data, time_frame) {
  # calculates volatility for a vector of days (loop so not super fast)
  # input: data: stock price data with at least open, high, low, close columns
  # input: time_frame: vector that holds the aggregation periods to calculate volatility (format c(start:end))
  # output: tibble with period and vola
  
  vola_vector <- c()
  for (i in seq_along(time_frame)){
    return_vola <- volatility(data[, c("open", "high", "low", "close")],
                              n = time_frame[i], calc = "close") %>%
      mean(na.rm = T)
    
    vola_vector[i] <- return_vola
  }
  vola_data <- tibble(period = time_frame,
                      djia_vola = vola_vector)
  
  return(vola_data)
}

my_vola_fun_progress <- function(data, time_frame) {
  # calculates volatility for a vector of days (loop so not super fast)
  # input: data: stock price data with at least open, high, low, close columns
  # input: time_frame: vector that holds the aggregation periods to calculate volatility (format c(start:end))
  # output: tibble with period and vola
  
  vola_vector <- c()
  
  withProgress(message = "Making plot", value = 0, {
    
    # number of times we go through the loop
    n <- time_frame[length(time_frame)] - time_frame[1] + 1
    
    # go through the loop
    for (i in seq_along(time_frame)){
      
      # add data in every step
      return_vola <- volatility(data[, c("open", "high", "low", "close")],
                                n = time_frame[i], calc = "close") %>%
        mean(na.rm = T)
      
      vola_vector[i] <- return_vola
      
      # update the progress bar
      incProgress(1/n, detail = paste("Doing part", i))
    }
    
    
  })
  
  vola_data <- tibble(period = time_frame,
                      djia_vola = vola_vector)
  
  return(vola_data)
}

## plots ----------------------

# descriptive plots

plot_descriptives_quarterly <- function(data_stock, data_gdp) {
  
  data_djia <- data_stock %>% tq_transmute(mutate_fun = to.quarterly) %>% 
    select(date, adjusted)
  
  data_gdp <- data_gdp %>% tq_transmute(mutate_fun = to.quarterly)
  
  data_plot <- left_join(data_gdp, data_djia) %>% 
    rename("GDP in billion dollar"  = price, "DJIA price" = adjusted) %>% 
    gather(key = "variable", value = "value", -date)
  
  ggplot(data = data_plot, aes(x = date, y = value, color = variable)) +
    geom_line() +
    theme_classic() +
    scale_color_manual(values = c("#1B9E77", "#7570B3"))
  
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

plot_descriptives_return <- function(data_stock, mean_type) {
  
  data_plot <- djia_prepare_daily(data_stock, mean_type)
  
  ggplot(data_plot, aes(x = date, y = daily.returns)) +
    geom_line(color = "#8c8c8c") + #"#1B9E77"
    ylab("DJIA return") +
    theme_classic()
  
}


# volatility plots

plot_djia_volatility_daily <- function(data, time_frame) {
  # creates a plot of volatility against aggregation period
  # input: djia raw data, mean_type ("log"/"arithmetic"), time_frame
  # output: ggplot
  # note: mean_type does not matter here
  
  data_plot <- data %>% 
    my_vola_fun_progress(time_frame)
  
  ggplot(data_plot, aes(x = period, fill = djia_vola, y = djia_vola)) +
    geom_area(fill = "#8DD3C7", color = "#1B9E77") +
    #geom_line() +
    xlab("aggregation horizon (days)") + 
    ylab("DJIA closing price volatility") +
    coord_cartesian(ylim = c(min(data_plot$djia_vola), max(data_plot$djia_vola)),
                    xlim = c(time_frame[1], tail(time_frame,1))) +
    theme_classic()

}

plot_djia_volatility_quarterly <- function(data, time_frame) {
  # creates a plot of volatility against aggregation period
  # input: djia raw data, mean_type, time_frame
  # output: ggplot
  
  data_plot <- data %>% 
    tq_transmute(select = c(date, open, high, low, close),
                 mutate_fun = to.quarterly) %>% 
    my_vola_fun_progress(time_frame)
  
  ggplot(data_plot, aes(x = period, fill = djia_vola, y = djia_vola)) +
    geom_area(fill = "#8DD3C7", color = "#1B9E77") +
    #geom_line() +
    xlab("aggregation horizon (quarters)") + 
    ylab("DJIA volatility") +
    coord_cartesian(ylim = c(min(data_plot$djia_vola), max(data_plot$djia_vola)),
                    xlim = c(time_frame[1], tail(time_frame,1))) +
    theme_classic()
  
}



my_regression <- function(data_djia, data_gdp) {
  
  data_djia <- data_djia %>% 
    tq_transmute(select = c(date, open, high, low, close),
                 mutate_fun = to.quarterly) %>% 
    select(c("date", "close"))
  
  data_gdp <- data_gdp %>% 
    mutate(date = as.yearqtr(date)) %>% 
    select(c("date", "price"))
  
  data_plot <- inner_join(data_djia, data_gdp, by = "date")
  
  reg_plot <- ggplot(data_plot, aes(x = price, y = close)) +
        geom_smooth(method = "lm", formula = "y ~ x") +
    geom_point(color = "#8c8c8c") + 
    xlab("GDP") + ylab("DJIA Close") +
    theme_classic()
  
  reg_model <- lm(data_plot$close ~ data_plot$price) %>% 
    summary()
  
  return(list(reg_plot, reg_model))
  
}

my_regression_growth <- function(data_djia, data_gdp) {
  
  data_djia <- data_djia %>% 
    tq_transmute(select = c(date, open, high, low, close),
                 mutate_fun = to.quarterly) %>% 
    select(c("date", "close"))
  
  data_gdp <- data_gdp %>% 
    mutate(date = as.yearqtr(date)) %>% 
    select(c("date", "price"))
  
  data_plot <- inner_join(data_djia, data_gdp, by = "date")
  
  reg_plot <- ggplot(data_plot, aes(x = price, y = close)) +
    geom_smooth(method = "lm", formula = "y ~ x") +
    geom_point(color = "#8c8c8c") + 
    xlab("GDP") + ylab("DJIA Close") +
    theme_classic()
  
  reg_model <- lm(data_plot$close ~ data_plot$price) %>% 
    summary()
  
  return(list(reg_plot, reg_model))
  
}





