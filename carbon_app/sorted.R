library(caTools)

## APP -----------

### VOLATILITY 

## Volatility not selective

sliderInput("slider_time_daily",
            label = "Select the time horizon",
            min = 5, max = 300, value = c(5,50),
),
h4("Market risk", em("(djia daily return volatility)")),
plotOutput("vola_plot_daily"),

## Volatility selective 

selectInput("select_volatility_aggregation",
            label = "Select the volatility aggregation level",
            choices = list("Quarterly volatiltiy" = "volatility_quarterly",
                           "Daily volatility" = "volatility_daily"),
            selected = "volatility_quarterly"
),

## Daily volatility
conditionalPanel(
  condition = "input.select_volatility_aggregation == 'volatility_daily'",
  sliderInput("slider_time_daily",
              label = "Select the time horizon",
              min = 5, max = 300, value = c(5,50),
  ),
  
  ### plot volatility
  h4("Market risk (daily)", em("(daily djia closing price volatility)")),
  plotOutput("vola_plot_daily"),
  
  ### message
  #h4(">> Analysis for daily data not done yet. <<", style = "color:blue"),
),

## Quarterly volatility
conditionalPanel(
  condition = "input.select_volatility_aggregation == 'volatility_quarterly'",
  
  ### plot volatility
  sliderInput("slider_time_quarterly",
              label = "Select the time horizon",
              min = 5, max = 135, value = c(5,50),
  ),
  h4("Market risk", em("(quarterly djia closing price volatility)")),
  plotOutput("vola_plot_quarterly"),
  
  
),



### HELPERS ------------

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

# vola functions 

my_vola_fun <- function(data, time_frame, return_period) {
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

my_vola_fun_manual <- function(data, time_frame, return_period) {
  # calculates volatility for a vector of days (loop so not super fast)
  # input: data: stock price data with at least open, high, low, close columns
  # input: time_frame: vector that holds the aggregation periods to calculate volatility (format c(start:end))
  # output: tibble with period and vola
  
  if(return_period == "quarterly"){
    data <- data %>% 
      tq_transmute(select = adjusted,
                   mutate_fun = periodReturn,
                   period = "quarterly",
                   type = "log") %>% 
      mutate("returns" = quarterly.returns)
  }
  
  if(return_period == "daily"){
    data <- data %>% 
      tq_mutate(select = adjusted,
                mutate_fun = periodReturn,
                period = "daily",
                type = "log") %>% 
      mutate("returns" = daily.returns)
  }
  
  vola_vector <- c()
  for (i in seq_along(time_frame)){
    return_vola <-  runsd(data$returns, k = time_frame[i], 
                         endrule = "NA", align = "right") %>%
      mean(na.rm = T)
    
    vola_vector[i] <- return_vola
  }
  vola_data <- tibble(period = time_frame,
                      djia_vola = vola_vector)
  
  return(vola_data)
}

# calculate quarterly returns from GDP scenarios

scenario_data <- scenarios %>% 
  scenario_prepare() %>% 
  select(-Model, - Region, - Unit, -Variable) %>%
       gather(key = "date", value = "value", - Scenario) %>% 
       mutate(date = ymd(date, truncated = 2L)) %>% 
  filter(Scenario == "NPi2020_400") %>% 
  tq_mutate(select = value,
            mutate_fun = quarterlyReturn,
            type = "log")

# problem - will have 20 times the same return

# alternative: find years with big drops in GDP and see how returns behaved in this period
test <- gdp %>%
  tq_mutate(select = price, 
            mutate_fun = quarterlyReturn) %>% 
  select(quarterly.returns) 

gdp_quantiles <- quantile(test$quarterly.returns, probs = c(0,0.05, 0.1))

gdp_drops <- gdp %>% 
  tq_mutate(select = price, 
            mutate_fun = quarterlyReturn) %>% 
  filter(quarterly.returns <= gdp_quantiles[2]) 

dates_gdp_drops1 <- seq(gdp_drops$date[1], by = "day", length.out = 300)

filtered_djia1 <- djia %>% 
  filter(date %in% dates_gdp_drops1)

### PLOTS ----------

plot_djia_volatility_quarterly <- function(data, time_frame) {
  # creates a plot of volatility against aggregation period
  # input: djia raw data, mean_type, time_frame
  # output: ggplot
  
  data_plot <- data %>% 
    tq_transmute(select = c(date, open, high, low, close, adjusted),
                 mutate_fun = to.quarterly) %>% 
    my_vola_fun_progress(time_frame)
  
  ggplot(data_plot, aes(x = period, fill = djia_vola, y = djia_vola)) +
    geom_area(fill = "#8DD3C7", color = "#1B9E77") +
    #geom_line() +
    xlab("aggregation horizon (quarters)") + 
    ylab("sd DJIA returns") +
    coord_cartesian(ylim = c(min(data_plot$djia_vola), max(data_plot$djia_vola)),
                    xlim = c(time_frame[1], tail(time_frame,1))) +
    theme_classic()
  
}