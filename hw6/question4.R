# Part A
library(BSDA)
library(coin)
observed <- 9
B <- 1500

los <- data.frame(
  'days' = c(3, 3, 4, 5, 5, 5, 6, 7, 7, 8, 9, 15, 6, 7, 7, 7, 8, 8, 8, 9, 9, 10, 10, 11, 13, 13, 15),
  'hospital' = c(rep("cauchy", 12), rep("skellam", 15))
)

cauchy_test <- SIGN.test(los[los$hospital == "cauchy",]$days, m = observed)
skellam_test <- SIGN.test(los[los$hospital == "skellam",]$days, m = observed)

# Part B
par(mfrow = c(1, 2))

hist(
  los[los$hospital == "cauchy",]$days,
  xaxt = 'n',
  breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16),
  main = 'Median LOS Bayesitis, Cauchy General',
  xlab = 'days',
  ylab = 'count'
)
axis(1, at = seq(0, 16, by = 2), labels = seq(0, 16, by = 2))

hist(
  los[los$hospital == "skellam",]$days,
  xaxt = 'n',
  breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16),
  main = 'Median LOS Bayesitis, Cauchy General',
  xlab = 'days',
  ylab = 'count'
)
axis(1, at = seq(0, 16, by = 2), labels = seq(0, 16, by = 2))

# Part C
mann_whitney <- wilcox.test(los$days ~ los$hospital)
mann_whitney$p.value
