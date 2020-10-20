setwd("C:/Users/froelijo/dev/methods1/hw6")
procedure <- read.csv("ProcedureCost.csv")

# Part A
observed <- mean(procedure[procedure$Procedure == 1,]$Cost) - mean(procedure[procedure$Procedure == 2,]$Cost)

n1 <- dim(procedure[procedure$Procedure == 1,])[1]
n2 <- dim(procedure[procedure$Procedure == 2,])[1]

B <- 10000

boot_ratio <- vector("double", B)
set.seed(8675309)

for (i in 1:B) {
  val1 <- sample(procedure$Cost, n1, replace = FALSE)
  val2 <- sample(procedure$Cost, n2, replace = FALSE)
  
  boot_ratio[i] <- mean(val1) - mean(val2)
}

hist(boot_ratio, main = "Bootstrap of Permutation of Costs", breaks = 50)
abline(
  v = mean(procedure[procedure$Procedure == 1,]$Cost) - mean(procedure[procedure$Procedure == 2,]$Cost),
  col = 'blue'
)

# Part B
P_right <- (sum(boot_ratio >= observed) + 1) / (B + 1)
p_left <- (sum(boot_ratio <= -observed) + 1) / (B + 1)

# Part C

