# get daily djia ticker form yahoo finance
djia <- tq_get("DJIA", get = "stock.prices", from = "2010-01-01")

# calculate returns
djia <- djia %>% tq_mutate(select = adjusted,
                          mutate_fun = periodReturn,
                          period = "daily",
                          type = "arithmetic") 

# simulation parameters
s0 <- tail(djia$adjusted,1) # initial stock price for simulation
dt <- 1 # time step
sim_range <- tibble("my_date" = seq(ymd(tail(djia$date,1)), ymd('2030-12-31'), by = '1 day')) %>% 
  filter(!wday(my_date) %in% c(1,7)) %>% 
  slice(-1) # prediction horizon
sim_length <- nrow(sim_range) # length of prediction horizon
time_position <- 1:sim_length # position in time as delta to last data point
sim_steps <- sim_length/dt # number of time points in prediction horizon

mu <- 0.000001
  #mean(djia$daily.returns)
sig <- 0.003
  #sd(djia$daily.returns)
nsim <- 10

# simulate returns
X <- matrix(rnorm(n = nsim * sim_steps, mean = 0, sd = 1), nsim, sim_steps) %>% 
  apply(1, cumsum) %>% 
  t()

my_drift <- matrix((mu - 0.5 * sig**2)*time_position)
my_diffusion <- sig * X

my_stock <- apply(my_diffusion, 1, function(x,drift) s0*exp(x+drift), drift = my_drift) %>% 
  t()

total_range <- rbind(tibble("my_date" = djia$date), sim_range)
total_stock <- cbind(t(matrix(rep(djia$adjusted,nsim), length(djia$adjusted), nsim)), my_stock)

plot(total_range, total_stock[1,], xlab = "time", ylim = c(0,45000), ylab = "DJIA Index", type = "l")
apply(total_stock[2:nsim,], 1, function(x,t) lines(t,x), t = total_range$my_date)

#plot(sim_range, X[1,], xlab = "time", ylab = "phenotype", type = "l")
#apply(X[2:nsim,], 1, function(x,t) lines(t,x), t = sim_range$my_date)
