# library(caTools)
# update app: rsconnect::deployApp()

## APP -----------

### VOLATILITY 

## Volatility not selective (don't choose between daily and quarterly)

sliderInput("slider_time_daily",
            label = "Select the time horizon",
            min = 5, max = 300, value = c(5,50),
),
h4("Market risk", em("(djia daily return volatility)")),
plotOutput("vola_plot_daily"),

## Volatility selective (choose between daily and quarterly)

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

# prepare djia data (took it out because it is so short, just do it manually whenever I need it)

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

# should be the same as the vola function in the helpers.R but without progress tracking, so that I can use it easier here
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

# check if manually calculating the volatility gives a similar result than R volatility() function
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

# calculate quarterly returns from GDP scenarios (unfinished)
scenario_returns <- function(scenario_data){
  scenario_data <- scenarios %>% 
    scenario_prepare() %>% 
    select(-Model, - Region, - Unit, -Variable) %>%
    gather(key = "date", value = "value", - Scenario) %>% 
    mutate(date = ymd(date, truncated = 2L)) %>% 
    filter(Scenario == "NPi2020_400") %>% 
    tq_mutate(select = value,
              mutate_fun = quarterlyReturn,
              type = "log")
}

# problem - will have 20 times the same return

# alternative: analyze quarters with big GDP drop separately

### PLOTS ----------

# set parameters manually
data_djia <- djia
data_gdp <- gdp
selected_quantile <- 0.5
time_frame <- c(5:50)
return_period <- "daily"

# same as in the helpers.R but with non-progress tracking volatility function
# careful: quite slow
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
        do(quarter_vola = my_vola_fun(., time_frame, return_period)) %>% 
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

# don't need that because I have one volatility function in which you can select if you want quarterly or daily
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

## old code -----------

data_plot_loss <- data_djia %>%  
  mutate(in_loss = as.yearqtr(date) %in% gdp_drops$year_quarter) %>% 
  filter(in_loss == TRUE) %>% 
  group_by(as.yearqtr(date)) %>% 
  filter(n() > 50) %>% 
  do(quarter_vola = my_vola_fun(., time_frame, return_period)) %>% 
  unnest(quarter_vola) %>% 
  group_by(period) %>% 
  summarize(djia_vola = mean(djia_vola))

data_djia %>% 
  mutate(in_loss = as.yearqtr(date) %in% gdp_drops$year_quarter) %>% 
  filter(in_loss == FALSE) %>% 
  group_by(as.yearqtr(date)) %>%
  filter(n() > 50) %>% 
  do(quarter_vola = my_vola_fun(., time_frame, return_period)) %>% 
  unnest(quarter_vola) %>% 
  group_by(period) %>% 
  summarize(djia_vola = mean(djia_vola))