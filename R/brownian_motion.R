t <- 0:100  # time
sig2 <- 0.01
my_mean <- 0

## first, simulate a set of random deviates
x <- rnorm(n = length(t) - 1, sd = sqrt(sig2))
## now compute their cumulative sum
x <- c(my_mean, cumsum(x))
plot(t, x, type = "l", ylim = c(-2, 2))

# simulate a bunch of brownian motions with same variables
nsim <- 50
# create a matrix with simulation data (rows: simulations, columns: time)
X <- matrix(rnorm(n = nsim * (length(t) - 1), sd = sqrt(sig2)), nsim, length(t) - 1)
# add zero column in front, take cumulative sum over time (transpose because cumsum changes rows and cols)
X <- cbind(rep(my_mean, nsim), t(apply(X, 1, cumsum)))
plot(t, X[1, ], xlab = "time", ylab = "phenotype", ylim = c(-2, 2), type = "l")
apply(X[2:nsim, ], 1, function(x, t) lines(t, x), t = t)

