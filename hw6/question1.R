setwd("C:/Users/froelijo/dev/methods1/hw6")
procedure <- read.csv("ProcedureCost.csv")
head(procedure)

# Part A
new <- procedure[procedure$Procedure == 2,]

par(mfrow = c(2,2))

hist(new$Cost, main = "Histogram of New Procedure Cost", breaks = 12, xlab = "thousands of dollars")
qqnorm(new$Cost)
qqline(new$Cost)
boxplot(new$Cost, ylab = "thousands of dollars")

# Part B

# Part C
mean <- mean(new$Cost)
min <- min(new$Cost)
first_quarter <- quantile(new$Cost, 0.25)
median <- median(new$Cost) 
third_quarter <- quantile(new$Cost, 0.75)
max <- max(new$Cost)
sd <- sd(new$Cost)
var <- var(new$Cost)

# Part D
n <- dim(new)[1]
B <- 10000

boot_vec <- vector("double", B)

set.seed(8675309)

for (i in 1:B) {
  val <- sample(new$Cost, n, replace = TRUE)
  boot_vec[i] <- mean(val)
}

par(mfrow = c(1, 2))
hist(boot_vec, main = "Bootstrap of New Cost", breaks = 50)
qqnorm(boot_vec)
qqline(boot_vec)

# Part E

# Part F
boot_mean <- mean(boot_vec)
boot_bias <- boot_mean - mean
boot_se <- sd(boot_vec)

# Part G
alpha = 0.05

normal_lower <- boot_mean - qnorm(1 - (alpha / 2)) * boot_se
normal_upper <- boot_mean + qnorm(1 - (alpha / 2)) * boot_se

coverage_lower <- sum(boot_mean < normal_lower) / B
coverage_upper <- sum(boot_mean > normal_upper) / B

boot_lower <- quantile(boot_mean, 0.025)
boot_upper <- quantile(boot_mean, 0.975)

accuracy <- boot_bias / boot_se
