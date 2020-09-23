# Part A
N = 1000
mmv_mat <- matrix(NA, nrow = N, ncol = 3)

for (i in 1:N) {
  rand_norm <- rnorm(10, 40, 10)
  mmv_mat[i, 1] <- mean(rand_norm)
  mmv_mat[i, 2] <- median(rand_norm)
  mmv_mat[i, 3] <- var(rand_norm)
}

hist(mmv_mat[,1], main = "1000 simulated means")
hist(mmv_mat[,2], main = "1000 simulated medians")
hist(mmv_mat[,3], main = "1000 simulated Variances")

# Part B

# Part C
n = 10
sigma <- 10
var <- mmv_mat[,3] * (n - 1) / sigma ** 2

hist(
  var,
  breaks = 35,
  probability = TRUE,
  main = "theoretical distribution and sample variances",
  col = 'red'
)

lines(
  curve(
    dchisq(x, n - 1),
    min(var) - 1,
    max(var) + 1,
    add = TRUE
  ),
  col = 'blue',
  lwd = 2
)

