# Data
hw7 <- data.frame(
  id = 1:7,
  gender = c(0,1,0,1,0,1,0),
  chol = c(254,402,288,354,220,451,405),
  wtkg = c(57,79,63,84,30,76,65),
  age = c(23,57,28,46,34,57,52)
)

# Question 2
# Part A

# Linear regression cholesterol and weight
fit <- lm(chol ~ wtkg, data = hw7)

# Info for table
sum <- summary(fit)
confint(fit)

# Part B
# y = b_0 + b_1(X) + e
# y = 97.395 + 3.727X

# Part C
# 97.395
# Average cholesterol if weight is zero kilograms (probably not interpretable)

# Part D
# [-133.3957971, 328.186461]
# we are 95% confident that cholesterol levels for persons with weight of zero
# kilograms is in the confidence interval [-133.3957971, 328.186461].

# Part E
# Fail to reject the hypothesis that the true intercept is 0, based on our 
# confidence interval [-133.3957971, 328.186461], which contains 0.

# Part F
# Estimated slope: 3.727
# For a unit increase in weight, for persons with weight between 30 and 84 kilograms,
# average cholesterol increases by 3.727 mg/100ml.

# Part G
# 95 % confidence interval: [0.2823573, 7.172412]
# Reject the hypthesis that the average slope is 0. Confidence interval does not contain zero.

# Part
library(tidyverse)
library(ggplot2)

plot(hw7$wtkg, hw7$chol, type = "p")

ggplot(data = hw7, mapping = aes(x = wtkg, y = chol, color = as.factor(gender))) +
  geom_point() +
  geom_abline(intercept = 97.395, slope = 3.727) +
  geom_abline(intercept = -133.43957971, slope = 0.2823573) +
  geom_abline(intercept = 328.186461, slope = 7.172412) +
  scale_x_continuous(limits = c(0, 200)) +
  scale_y_continuous(limits = c(-500, 1000))

predict(fit, hw7, interval = "predict", level = )



