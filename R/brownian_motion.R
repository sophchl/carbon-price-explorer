## simulate brownian motion path

## set variables
t <- 0:100
sig2 <- 0.01
mu <- 0

## first, simulate a set of random deviates
bm <- rnorm(n = length(t) - 1, sd = sqrt(sig2))
## now compute their cumulative sum
bm <- c(mu, cumsum(bm))
plot(t, bm, type = "l", ylim = c(-2, 2))

# simulate a bunch of brownian motions with same variables
nsim <- 50
# create a matrix with simulation data (rows: simulations, columns: time)
bm_matrix <- matrix(rnorm(n = nsim * (length(t) - 1),
    sd = sqrt(sig2)), nsim, length(t) - 1)
# add zero column in front, take cumulative sum over time
# transpose because cumsum changes rows and cols
bm_matrix <- cbind(rep(mu, nsim), t(apply(bm_matrix, 1, cumsum)))
plot(t, bm_matrix[1, ], type = "l", ylim = c(-2, 2),
    xlab = "time", ylab = "BM path")
apply(bm_matrix[2:nsim, ], 1, function(x, t) lines(t, x), t = t)
