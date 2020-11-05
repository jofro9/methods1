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
fit_glm <- glm(chol ~ wtkg, data = hw7)
fit_lm <- lm(chol ~ hw7$wtkg, data = hw7) # need to use lm instead of glm

# Info for table
sum <- summary(fit_glm)
sum
sum$Coefficients[1, 1]

ci <- confint(fit_glm)
ci

ci_lm <- confint(fit_lm)
ci_lm

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
# We can say with 95% confidence that the average increase in Cholesterol is between 0.28 and 7.17 for a unit increase in weight.

# Part H
# The 95% confidence interval for the slope of the regression line of weight and cholesterol does not contain 0. Therefore, we reject that hypothesis that the true slope of the regression line of weight and cholesterol is equal to 0.

# Part I
# There is a significant increase in plasma levels of total cholesterol (mg/100mL) for increasing weight (kg) in individuals (p = 0.388). On average, the plasma levels of total cholesterol increases by 3.727 mg/100mL (95% CI: 0.282 to 7.17 mg/100mL) for every 1 kg increase in weight.

# Part J
library(tidyverse)
library(ggplot2)

plot(hw7$wtkg, hw7$chol, type = "p")

pred <- predict(fit_lm, newdata = data.frame(hw7_new=50), interval='prediction')
hw7_new <- cbind(hw7, pred)

ggplot(data = hw7_new, aes(x = wtkg, y = chol))+
  geom_point() +
  geom_line(aes(y = lwr), color = "red", linetype = "twodash")+
  geom_line(aes(y = upr), color = "red", linetype = "twodash")+
  geom_smooth(method = lm, se = TRUE) +
  labs(x='Weight (kg)', y='Cholesterol (mg/100mL)') +
  theme(legend.position = "right")

