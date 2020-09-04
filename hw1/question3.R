# Joseph Froelicher
# Dr. Alex Kaizer
# BIOS 6611
# 09/10/2020

### Problem 3 ###

# Part A #
set.seed(8675309)

height <- rnorm(100, 70, sqrt(15))

bias <- median(height) - 70

# Part B & C #
set.seed(8675309)
vec.length <- 100000/100
bias.mat <- matrix(NA, vec.length, 4)
iter <- 0
mean <- 70
sd <- sqrt(15)

for ( i in seq(100, 100000, 100) ) {
  iter <- iter + 1
  
  height <- rnorm(i, mean, sd)
  median <- median(height)
  
  median.vec <- rep(median, i)
  
  bias.mat[iter, 1] <- i / 100
  bias <- median - mean
  bias.mat[iter, 2] <- bias
  sum <- 0
  
  for ( h in height ) {
    sum <- sum + ( ( h - median ) ** 2 )
  }
  
  bias.mat[iter, 3] <- sum / length(height)
  bias.mat[iter, 4] <- iter * 100
}

head(bias.mat)

plot(
  bias.mat[,4],
  bias.mat[,2],
  main = "Measure of Bias from n = 100 to n = 100,000, by increments of 100",
  xlab = "n",
  ylab = "Bias",
  pch = 20
)

plot(
  bias.mat[,4],
  bias.mat[,3],
  main = "Variance WRT the Median estimator, from n = 100 to n = 100,000, by increments of 100",
  xlab = "n",
  ylab = "Variance WRT the Median",
  pch = 20
)


# Part D #
set.seed(8675309)
efficiency.mat <- matrix(NA, 10000, 5)

for ( i in seq(1, 10000, 1) ) {
  height <- rnorm(1000, mean, sd)
  
  efficiency.mat[i, 1] <- mean(height)
  efficiency.mat[i, 3] <- median(height)
  
  mean.var <- 0
  median.var <- 0
  
  mean.e <- mean(height)
  median.e <- median(height)
  
  for ( h in height ) {
    mean.var <- mean.var + ( ( h - mean.e ) ** 2 )
    median.var <- median.var + ( ( h - median.e ) ** 2 )
  }
  
  mean.var <- mean.var / length(height)
  median.var <- median.var / length(height)

  efficiency.mat[i, 2] <- mean.var
  efficiency.mat[i, 4] <- median.var
  efficiency.mat[i, 5] <- i
}

plot(
  efficiency.mat[,5],
  efficiency.mat[,2]
)

plot(
  efficiency.mat[,5],
  efficiency.mat[,4]
)
