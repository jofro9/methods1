# Question 4

# Parts A & B
# individual sample size
n <- c(10, 50, 100, 1000)

# the number of simulations per sample size
N <- 500

# number of columns in the matrices
C <- 4

sample_mean <- matrix(NA, N, C)

iter <- 0

for (j in 1:length(n)) {
  for (i in 1:N) {
    draws <- rcauchy(n[j])
    sample_mean[i, j] <- mean(draws)
  }
}

# Part C
means_table <- data.frame(
  mean(sample_mean[,1]),
  mean(sample_mean[,2]),
  mean(sample_mean[,3]),
  mean(sample_mean[,4])
)

colnames(means_table) <- c('n = 10', 'n = 50', 'n = 100', 'n = 1000')
means_table

sd_table <- data.frame(
  sd(sample_mean[,1]),
  sd(sample_mean[,2]),
  sd(sample_mean[,3]),
  sd(sample_mean[,4])
)

colnames(sd_table) <- c('n = 10', 'n = 50', 'n = 100', 'n = 1000')
sd_table

# Part D
par(mfrow = c(2,2))

hist(
  sample_mean[,1],
  breaks = 50,
  main = 'Sample means n = 10',
  xlab = expression(mu),
  col = 'blue'
)

hist(
  sample_mean[,2],
  breaks = 50,
  main = 'Sample means n = 50',
  xlab = expression(mu),
  col = 'red'
)

hist(
  sample_mean[,3],
  breaks = 50,
  main = 'Sample means n = 100',
  xlab = expression(mu),
  col = 'orange'
)

hist(
  sample_mean[,4],
  breaks = 50,
  main = 'Sample means n = 1000',
  xlab = expression(mu),
  col = 'yellow'
)
