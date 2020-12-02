setwd("C:/Users/froelijo/dev/methods1/hw10")
lead = read.csv('lead2.csv')

# Part A
fit1 = lm(iq ~ expose, data = lead)
fit1$coefficients[2]

# Part B
fit2 = lm(iq ~ expose + sex, data = lead)
fit2$coefficients[2]

# Part C
fit3 = lm(sex ~ expose, data = lead)
summary(fit3)
(fit1$coefficients[2] - fit2$coefficients[2]) / fit1$coefficients[2] * 100
