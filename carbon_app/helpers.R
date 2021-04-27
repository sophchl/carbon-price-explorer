# scenarios <- read.csv(file = "carbon_app/data/cd-links-gdp-us.csv")
# djia <- tq_get("DJIA", get = "stock.prices", from = "1930-01-01")
# gdp <- tq_get("GDP", get = "economic.data", from = "1930-01-01")
# app_diretory: 'C:/Users/Sophia/Dropbox/01_Studium/04_projects/CarbonPriceExplorer/carbon_app'

## data transformations ------------------

# prepare scenario data

scenario_prepare <- function(data) {
  # remove strings from column names and remove last row with source information
  # input: data: scenario data from iiasa
  
  colnames(data) <- c(colnames(data)[0:5],
                      substr(colnames(data[6:ncol(data)]), start = 2, stop = 5))
  data <- data %>% slice(seq_len(nrow(data) - 1))
  
  return(data)
}

# calculate volatility for selected days

my_vola_fun_progress <- function(data, time_frame, return_period) {
  # calculates volatility for a vector of days (loop so not super fast)
  # input: data: stock price data with at least open, high, low, close columns
  # input: time_frame: vector that holds the aggregation periods to calculate volatility (format c(start:end))
  # output: tibble with period and vola
  
  if(return_period == "quarterly"){
    data <- data %>% 
      tq_transmute(select = c(date, open, high, low, close, adjusted),
                   mutate_fun = to.quarterly)
  }
  
  if(return_period == "daily"){
    data <- data
  }
  
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

# calculate GDP quantiles

quantile_analysis <- function(data_gdp){
  # returns tibble with GDP quantiles
  
  my_quantiles <- c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.9, 1)
  gdp_quantiles <- data_gdp %>%
    tq_mutate(select = price, mutate_fun = quarterlyReturn) %>% 
    filter(quarterly.returns < 0) %>% 
    summarize(for_quantile = my_quantiles,
              gdp_quantile = quantile(quarterly.returns, probs = my_quantiles))
  
  return(gdp_quantiles %>% rename("GDP loss" = gdp_quantile, "p-quantile" = for_quantile))
  
}

## plots ----------------------

# descriptive plots

plot_descriptives_quarterly <- function(data_stock, data_gdp) {
  
  data_djia <- data_stock %>% tq_transmute(mutate_fun = to.quarterly) %>% 
    select(date, adjusted)
  
  data_gdp <- data_gdp %>% tq_transmute(mutate_fun = to.quarterly)
  
  data_plot <- left_join(data_gdp, data_djia, by = "date") %>% 
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
  
  data_plot <- data_stock %>% 
    tq_mutate(select = adjusted,
              mutate_fun = periodReturn,
              period = "daily",
              type = mean_type)
  
  ggplot(data_plot, aes(x = date, y = daily.returns)) +
    geom_line(color = "#8c8c8c") + #"#1B9E77"
    ylab("DJIA return") +
    theme_classic()
  
}


# volatility plots

plot_djia_volatility <- function(data, time_frame, return_period) {
  # creates a plot of volatility against aggregation period
  # input: djia raw data, mean_type ("log"/"arithmetic"), time_frame, return_period: daily or quarterly
  # output: ggplot
  # note: mean_type does not matter here
  
  data_plot <- data %>% 
    my_vola_fun_progress(time_frame, return_period)
  
  ggplot(data_plot, aes(x = period, fill = djia_vola, y = djia_vola)) +
    geom_area(fill = "#8DD3C7", color = "#1B9E77") +
    #geom_line() +
    xlab(paste("aggregation horizon ", "(", return_period, ")", sep = "")) + 
    ylab("sd DJIA returns") +
    coord_cartesian(ylim = c(min(data_plot$djia_vola), max(data_plot$djia_vola)),
                    xlim = c(time_frame[1], tail(time_frame,1))) +
    theme_classic()

}

plot_djia_volatility_separate <- function(data_djia, data_gdp, selected_quantile, time_frame, return_period) {
  
  # calculate GDP quantiles
  gdp_quantiles <- quantile_analysis(data_gdp) %>% 
    rename(gdp_quantile = "GDP loss", for_quantile = "p-quantile")
  
  # find quarters with GDP losses lower than selected quantile
  gdp_drops <- data_gdp %>% 
    tq_mutate(select = price, mutate_fun = quarterlyReturn) %>% 
    filter(quarterly.returns <= gdp_quantiles %>% 
             filter(for_quantile == selected_quantile) %>% 
             pull(gdp_quantile)) %>% 
    mutate(year_quarter = as.yearqtr(date)) 
  
  # create two separate data frames for loss and non-loss quarters
  filter_function <- function(data_djia, gdp_drops, boolean_in_loss){
    data_djia <- data_djia %>% 
      mutate(in_loss = as.yearqtr(date) %in% gdp_drops$year_quarter) %>% 
      filter(in_loss == boolean_in_loss) %>% 
      group_by(as.yearqtr(date)) %>% 
      filter(n() > 50) %>% 
      do(quarter_vola = my_vola_fun_progress(., time_frame, return_period)) %>% 
      unnest(quarter_vola) %>% 
      group_by(period) %>% 
      summarize(djia_vola = mean(djia_vola))
    
    return(data_djia)
  }
  
  
  data_plot_loss <- filter_function(data_djia, gdp_drops, TRUE)
  data_plot_no_loss <- filter_function(data_djia, gdp_drops, FALSE)
  
  # plot both curves
  ggplot() +
    geom_area(data = data_plot_loss, aes(x = period, y = djia_vola, fill = "in loss period"), 
              color = "#dcedc1", alpha = 0.5) +
    geom_area(data = data_plot_no_loss, aes(x = period, y = djia_vola, fill = "outside loss period"), 
              color = "#a8e6cf", alpha = 0.5) +
    xlab(paste("aggregation horizon ", "(", return_period, ")", sep = "")) + 
    ylab("sd DJIA returns") +
    coord_cartesian(ylim = c(min(data_plot_no_loss$djia_vola), max(data_plot_loss$djia_vola)),
                    xlim = c(time_frame[1], tail(time_frame,1))) +
    scale_fill_manual(name = "Legend", values = c("#dcedc1", "#a8e6cf")) +
    theme_classic()
  
}

# regression plots and summary

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
  
  reg_model <- lm(close ~ price, data = data_plot) %>% 
    summary()
  
  return(list(reg_plot, reg_model))
  
}

my_regression_growth <- function(data_djia, data_gdp, remove_outlier) {
  
  data_djia <- data_djia %>% 
    tq_transmute(select = c(date, open, high, low, close),
                 mutate_fun = to.quarterly) %>% 
    mutate(return = (close - lag(close))/lag(close)) %>% 
    select(c("date", "return"))
  
  data_gdp <- data_gdp %>% 
    mutate(date = as.yearqtr(date)) %>% 
    mutate(growth = (price - lag(price))/lag(price)) %>% 
    select(c("date", "growth")) 
  
  if(remove_outlier == TRUE){
    data_gdp <- data_gdp %>% 
      filter(growth <= 0.05 & growth >= -0.05)
  }
  
  data_plot <- right_join(data_djia, data_gdp, by = "date") %>% 
    slice(-1)
  
  reg_plot <- ggplot(data_plot, aes(x = growth, y = return)) +
    geom_smooth(method = "lm", formula = "y ~ x") +
    geom_point(color = "#8c8c8c") + 
    xlab("GDP growth") + ylab("DJIA return") +
    theme_classic()
  
  reg_model <- lm(return ~ growth, data = data_plot) %>% 
    summary()
  
  return(list(reg_plot, reg_model))
  
}





