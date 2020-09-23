# Part A
set.seed(8675309)

rand.norm <- rnorm(10000, 125, 8)
rand.exp <- rexp(10000, 1.5)

# Part B
rand.norm.mean <- mean(rand.norm)
rand.norm.sd <- sd(rand.norm)

rand.exp.mean <- mean(rand.exp)
rand.exp.sd <- sd(rand.exp)

# Part C
hist(rand.norm, main = "Random Normal with n = 10,000")
boxplot(rand.norm, main = "Random Normal with n = 10,000")

hist(rand.exp, main = "Random Exponential with n = 10,000")
boxplot(rand.exp, main = "Random Exponential with n = 10,000")