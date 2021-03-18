## look at djia index data

# get daily djia ticker from yahoo finance
djia <- tq_get("DJIA", get = "stock.prices", from = "2010-01-01")

# calculate returns
djia <- djia %>% tq_mutate(select = adjusted,
                          mutate_fun = periodReturn,
                          period = "daily",
                          type = "arithmetic")

# simulation parameters
s0 <- tail(djia$adjusted, 1) # initial stock price for simulation
dt <- 1 # time step
sim_range <- tibble("sim_range" = seq(ymd(tail(djia$date, 1)),
  ymd("2030-12-31"), by = "1 day")) %>%
  filter(!wday(sim_range) %in% c(1, 7)) %>%
  slice(-1)  # prediction horizon

sim_length <- nrow(sim_range) # length of prediction horizon
time_position <- 1:sim_length # position in time as delta to last data point
sim_steps <- sim_length / dt # number of time points in prediction horizon

mu <- 0.000001
# alternatively: mean(djia$daily.returns)
sig <- 0.003
# alternatively: sd(djia$daily.returns)
nsim <- 10

# simulate returns
ret_sim <- matrix(rnorm(n = nsim * sim_steps, mean = 0, sd = 1),
  nsim, sim_steps) %>%
  apply(1, cumsum) %>%
  t()

my_drift <- matrix((mu - 0.5 * sig**2) * time_position)
my_diffusion <- sig * ret_sim

my_stock <- apply(my_diffusion, 1,
  function(x, drift) s0 * exp(x + drift), drift = my_drift)

total_stock_data <- tibble(
  "date" = append(djia$date, sim_range$sim_range),
  "dow_historic" = c(djia$adjusted, rep(NA, length(sim_range$sim_range))),
  ) %>%
  cbind(
      rbind(matrix(data = NA, nrow = length(djia$adjusted), ncol = nsim),
      my_stock) %>%
      as_tibble(.name_repair = c("unique", "minimal", "universal"))
    )

total_stock_data  %>%
  gather(key = "dow_series", value = "value", -date)  %>%
  ggplot(aes(x = date, y = value)) +
  geom_path() +
  theme_classic()
