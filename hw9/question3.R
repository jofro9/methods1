setwd("C:/Users/froelijo/dev/methods1/hw9")

carotenoids <- read.table('carotenoids.dat')

colnames(carotenoids) <- c(
  'age',
  'sex',
  'smoke',
  'bmi',
  'vitamins',
  'calories',
  'fat',
  'fiber',
  'alcohol',
  'chol',
  'betadiet',
  'retdiet',
  'betaplas',
  'retplas'
)

# Part A
never_mean <- mean(carotenoids[carotenoids$smoke == 1,]$betaplas)
never_sd <- sd(carotenoids[carotenoids$smoke == 1,]$betaplas)
never_se <- never_sd / sqrt(length(carotenoids[carotenoids$smoke == 1,]$betaplas))

former_mean <- mean(carotenoids[carotenoids$smoke == 2,]$betaplas)
former_sd <- sd(carotenoids[carotenoids$smoke == 2,]$betaplas)
former_se <- never_sd / sqrt(length(carotenoids[carotenoids$smoke == 2,]$betaplas))

current_mean <- mean(carotenoids[carotenoids$smoke == 3,]$betaplas)
current_sd <- sd(carotenoids[carotenoids$smoke == 3,]$betaplas)
current_se <- never_sd / sqrt(length(carotenoids[carotenoids$smoke == 3,]$betaplas))

# Part B
carotenoids$never <- carotenoids$smoke == 1
carotenoids$former <- carotenoids$smoke == 2
carotenoids$current <- carotenoids$smoke == 3

fit_ref <- glm(betaplas ~ former + current, data = carotenoids)
summary(fit_ref)

# Part C
fit_empty <- glm(betaplas ~ 1, data = carotenoids)
fit_smokecal <- glm(betaplas ~ calories + smoke, data = carotenoids)
anova(fit_smokecal, fit_empty, test = "F")

# Part D

# Part E
fit_smoke <- glm(betaplas ~ smoke, data = carotenoids)
anova(fit_smoke, fit_empty, test = "F")
