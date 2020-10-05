# Part A
n <- 5
mu <- 0
sd <- 75
alpha <- 0.05
iter <- 10000
t_mat <- matrix(NA, iter, 3)
colnames(t_mat) <- c('test.statistic', 'p.value', 'reject')
set.seed(2345)

for (i in 1:iter) {
  vals <- rnorm(n, mu, sd)

  t_result <- t.test(
    x = vals,
    alternative = "two.sided",
    mu = mu,
    conf.level = 1 - alpha
  )
  
  t_mat[i, 1] <- t_result$statistic
  t_mat[i, 2] <- t_result$p.value
  
  if (t_result$p.value < alpha) {
    t_mat[i, 3] <- 1
  } else {
    t_mat[i, 3] <- 0
  }
}

sum(t_mat[,3]) / iter * 100

# Part B
# function definition
simNull <- function(n, mu, sd, alpha, iter) {
  power <- vector("double", iter)
  set.seed(8675309)
  
  for (i in 1:iter) {
    vals  <- rnorm(n, mu, sd)
    z_alpha <- qnorm(1 - (alpha / 2))
    delta <- abs(mu - mean(vals))
    se <- sd / sqrt(n)
    power[i] <- (delta / se) - z_alpha
  }
  
  mean_power <- (1 - pnorm(mean(power))) * 100
  median_power <- (1 - pnorm(median(power))) * 100
  
  return(data.frame('power mean' = mean_power, 'power median' = median_power))
}

# testing
simNull(5, 0, 75, 0.05, 10000)

# Part C
mu0 <- 0
mu1 <- 100
sd <- 75
n <- 5
alpha <- 0.05
iter <- 10000
power_vec <- vector("double", iter)

set.seed(1796)

for (i in 1:iter) {
  vals0 <- rnorm(n, mu0, sd)
  vals1 <- rnorm(n, mu1, sd)
  
  p <- power.t.test(n, mean(vals0) - mean(vals1), sd, alpha, power = NULL, type='one.sample')
  power_vec[i] <- p$power
}

mean(power_vec)

# Part D
# function definition
simAlternative <- function(n, mu0, mu1, sd, alpha, iter) {
  power <- vector("double", iter)
  set.seed(8675309)
  
  for (i in 1:iter) {
    vals0  <- rnorm(n, mu0, sd)
    vals1  <- rnorm(n, mu1, sd)
    
    z_alpha <- qnorm(1 - (alpha / 2))
    delta <- abs(mean(vals0) - mean(vals1))
    se <- sd / sqrt(n)
    power[i] <- (delta / se) - z_alpha
  }
  
  mean_power <- (1 - pnorm(mean(power))) * 100
  median_power <- (1 - pnorm(median(power))) * 100
  
  return(data.frame('power mean' = mean_power, 'power median' = median_power))
}

# testing
simAlternative(5, 0, 100, 75, 0.05, 10000)
