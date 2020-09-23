# Question 4

# Parts A & B
# individual sample size
n <- 10

# the number of simulations per sample size
N <- 500

# probability of success
P <- 0.15

# number of columns in the matrices
C <- 5

sample_mean <- matrix(NA, N, C)

iter <- 0

for (n in seq(10, 50, 10)) { # 1:5 == seq(1,5,1)
  iter <- iter + 1

  for (i in 1:N) {
    draws <- rbinom(n, 1, P)
    sample_mean[i, iter] <- mean(draws)
  }
}

# Part C
means_table <- data.frame(
  mean(sample_mean[,1]),
  mean(sample_mean[,2]),
  mean(sample_mean[,3]),
  mean(sample_mean[,4]),
  mean(sample_mean[,5])
)

colnames(means_table) <- c('n = 10', 'n = 20', 'n = 30', 'n = 40', 'n = 50')
means_table

sd_table <- data.frame(
  sd(sample_mean[,1]),
  sd(sample_mean[,2]),
  sd(sample_mean[,3]),
  sd(sample_mean[,4]),
  sd(sample_mean[,5])
)

colnames(sd_table) <- c('n = 10', 'n = 20', 'n = 30', 'n = 40', 'n = 50')
sd_table

# Part D
par(mfrow = c(2,3))

hist(
  sample_mean[,1],
  breaks = 15,
  main = 'Sampling distribution means, n = 10',
  xlab = expression(mu),
  col = 'blue'
)

hist(
  sample_mean[,2],
  breaks = 15,
  main = 'Sampling distribution means, n = 20',
  xlab = expression(mu),
  col = 'red'
)

hist(
  sample_mean[,3],
  breaks = 15,
  main = 'Sampling distribution means, n = 30',
  xlab = expression(mu),
  col = 'orange'
)

hist(
  sample_mean[,4],
  breaks = 15,
  main = 'Sampling distribution means, n = 40',
  xlab = expression(mu),
  col = 'yellow'
)

hist(
  sample_mean[,5],
  breaks = 15,
  main = 'Sampling distribution means, n = 50',
  xlab = expression(mu),
  col = 'green'
)

# Part E
