# Exercise 4

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
sample_sd <- matrix(NA, N, C)

iter <- 0

for (n in seq(10, 50, 10)) { # 1:5 == seq(1,5,1)
  iter <- iter + 1
  print(iter)
  
  for (i in 1:N) {
    draws <- rbinom(n, 1, P)
    sample_mean[i, iter] <- mean(draws)
    sample_sd[i, iter] <- sd(draws)

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
  sd(sample_sd[,1]),
  sd(sample_sd[,2]),
  sd(sample_sd[,3]),
  sd(sample_sd[,4]),
  sd(sample_sd[,5])
)

colnames(sd_table) <- c('n = 10', 'n = 20', 'n = 30', 'n = 40', 'n = 50')
sd_table
