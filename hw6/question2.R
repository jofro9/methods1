setwd("C:/Users/froelijo/dev/methods1/hw6")
procedure <- read.csv("ProcedureCost.csv")
head(procedure)
old <- procedure[procedure$Procedure == 1,]
new <- procedure[procedure$Procedure == 2,]
head(new)
head(old)

# Part A
n1 <- dim(new)[1]
n2 <- dim(old)[1]

B <- 10000

boot_ratio <- vector("double", B)

set.seed(8675309)

for (i in 1:B) {
  val1 <- sample(new$Cost, n1, replace = TRUE)
  val2 <- sample(old$Cost, n2, replace = TRUE)
  
  boot_ratio[i] <- mean(val1) - mean(val2)
}

par(mfrow = c(1, 2))
hist(boot_ratio, main = "Bootstrap of Ratio of Costs", breaks = 50)
qqnorm(boot_ratio)
qqline(boot_ratio)

# Part B

# Part C
boot_mean <- mean(boot_vec)
boot_bias <- boot_mean - mean
boot_se <- sd(boot_vec)

# Part D
alpha = 0.05

normal_lower <- boot_mean - qnorm(1 - (alpha / 2)) * boot_se
normal_upper <- boot_mean + qnorm(1 - (alpha / 2)) * boot_se

coverage_lower <- sum(boot_mean < normal_lower) / B
coverage_upper <- sum(boot_mean > normal_upper) / B

boot_lower <- quantile(boot_mean, 0.025)
boot_upper <- quantile(boot_mean, 0.975)

accuracy <- boot_bias / boot_se
