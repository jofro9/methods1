# Poisson Distribution
set.seed(8675309)
LAMBDA <- 1.5
rps <- rpois(10000, LAMBDA)

# theoretical mean and sd from Miller I and Miller M, 'John E. Freund's Mathematical Statistics with Applications'; eigth edition, p. 162
p.t_mean <- LAMBDA
p.t_sd <- LAMBDA
p.t_mean
p.t_sd

# simulated mean and sd
p.s_mean <- mean(rps)
p.s_sd <- sd(rps)
p.s_mean
p.s_sd

# histogram and boxplot
hist(
  rps,
  breaks = -2:8,
  main = "Histogram of Simulated Poisson distribution (N = 10,000)",
  xlab = "lambda",
  ylab = "n"
)

boxplot(
  rps,
  ylab = "counts",
  main = "Boxplot of Simulated Poisson distribution (n = 10,000)"
)

# Binomial distribution
set.seed(8675309)
N <- 5
THETA <- 0.15
rbs <- rbinom(10000, N, THETA)

# theoretical mean and sd from Miller I and Miller M, 'John E. Freund's Mathematical Statistics with Applications'; eigth edition, p. 149
b.t_mean <- N * THETA
b.t_sd <- sqrt( N * THETA * (1 - THETA) )
b.t_mean
b.t_sd

# simulated mean and sd
b.s_mean <- mean(rbs)
b.s_sd <- sd(rbs)
b.s_mean
b.s_sd

# histogram and boxplot
hist(
  rbs,
  breaks = -2:8,
  main = "Histogram of Simulated Binomial distribution (N = 10,000)",
  ylab = "n"
)

boxplot(
  rbs,
  ylab = "counts",
  main = "Boxplot of Simulated Binomial distribution (n = 10,000)"
)
