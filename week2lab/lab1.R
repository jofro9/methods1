plot(
  0:15,
  dpois(
    0:15,
    lambda=3.5
  ),
  type = 'h',
  ylab='Probability',
  xlab='Number of Occurrences',
  main="Poisson PMF"
)

# length of the vectors storing information (ie. number of simulations)
vec.length <- 100000/100

# empty matrix to store some information later on
bias.mat <- matrix(NA, vec.length, 5)

# constant lambda
LAMBDA <- 3.5

# iterator to keep location in matrices
iter <- 0
         
for ( N in seq(from = 100, to = 100000, by = 100) ) {
  # increase the iterator by 1 every time through the loop
  iter <- iter + 1
  
  # random poisson distrbution with sample size n 
  sim <- rpois(n = N, lambda = LAMBDA)
  
  # store the means in the first column of the matrix
  bias.mat[iter, 1] <- mean(sim)
  
  # store the variances in the second column
  bias.mat[iter, 2] <- var(sim)
  
  # store the sample size in the fifth column
  bias.mat[iter, 5] <- N
}

# store the mean bias in the third column
bias.mat[ ,3] <- bias.mat[ ,1] - LAMBDA

# store the variance bias in the third column
bias.mat[ ,4] <- bias.mat[ ,2] - LAMBDA

# give the columns informative titles
colnames(bias.mat) <- c("mean", "variance", "m.bias", "v.bias", "n")

# look at it
head(bias.mat)

# plot the mean bias
plot(
  bias.mat[,5], # n
  bias.mat[,3], # mean bias
  main = "Bias of the Mean Estimator", # title
  xlab = "n", # x axis title
  ylab = "Bias", # y axis title
  pch = 20 # size and shape of the points
)

# plot the variance bias
plot(
  bias.mat[,5],
  bias.mat[,4],
  main = "Bias of the Variance",
  xlab = "n",
  ylab = "Variance WRT the Variance",
  pch = 20
)
