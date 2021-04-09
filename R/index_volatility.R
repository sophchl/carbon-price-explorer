### look at djia index volatility

## plot djia return volatility at different aggregation horizons
## next step: compare to climate policy simulations

## Note; start with price, later do return

#--------------------------------------------------------------------

### djia index data ------

# get daily djia ticker from yahoo finance
djia <- tq_get("DJIA", get = "stock.prices", from = "1990-01-01")

# calculate returns
djia <- djia %>%
  tq_mutate(select = adjusted,
    mutate_fun = periodReturn,
    period = "daily",
    type = "arithmetic")  %>%
  mutate(date = ymd(date))  %>%
  select(-symbol)

lapply(djia, class)

volatility(djia[, c("open", "high", "low", "close")], n = 5, calc = "close") %>%
    mean(na.rm = T)

# calculate a function that calculates volatility for selected days
my_vola_fun <- function(data, n_days) {
    # calculates volatility for one number of days
    return_vola <- volatility(data[, c("open", "high", "low", "close")],
        n = n_days, calc = "close") %>%
    mean(na.rm = T)
    return(return_vola)
    }

my_vola_fun2 <- function(data, vector_days) {
    # calculates volatility for a vector of days (loop so not super fast)
    vola_vector <- c()
    for (i in seq_along(vector_days)){
        return_vola <- volatility(data[, c("open", "high", "low", "close")],
        n = vector_days[i], calc = "close") %>%
        mean(na.rm = T)

        vola_vector[i] <- return_vola
    }
    return(vola_vector)
    }


# apply to djia
my_time_frame <- c(5:200)
djia_vola <- djia %>% summarise(djia_vola = my_vola_fun2(djia, my_time_frame),
    period = my_time_frame)

ggplot(djia_vola, aes(period, djia_vola)) +
    geom_line() +
    theme_classic()

# plot
plot(djia_vola)
