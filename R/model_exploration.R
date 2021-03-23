## model 1 tbd

# data: djia, carbon_price

# simulate returns with carbon
my_stock_carbon <- my_stock

total_range <- rbind(tibble("my_date" = djia$date), sim_range)
total_stock <- cbind(t(matrix(rep(djia$adjusted, nsim),
    length(djia$adjusted), nsim)), my_stock)

plot(total_range, total_stock[1, ], xlab = "time",
    ylim = c(0, 5000), ylab = "DJIA Index", type = "l")
apply(total_stock[2:nsim, ], 1, function(x, t) lines(t, x),
    t = total_range$my_date)

## model 2: look at climate conferences