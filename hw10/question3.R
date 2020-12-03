library(tidyverse)

setwd("C:/Users/froelijo/dev/methods1/hw10")
lead = read.csv('lead2.csv')

# Part A
fit1 = lm(iq ~ miles + first2y + miles * first2y, data = lead)
fit1$coefficients

# Part B
summary(fit1)$coefficients[4, 4]

# Part C
fit1$coefficients

# Part D
fit1$coefficients

# Part E
lead$first2y = if_else(lead$first2y == 1, "Yes", "No")

ggplot(lead, aes(miles, iq, shape = factor(first2y), color = factor(first2y))) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", aes(color = factor(first2y))) +
  ggtitle("IQ Score by Distance from Smelter") +
  xlab("Distance (miles)") +
  ylab("IQ (score)") +
  scale_x_continuous(breaks = seq(0, 4, 0.5), limits = c(0, 4), minor_breaks = FALSE) +
  scale_y_continuous(breaks = seq(50, 150, 10), limits = c(50, 150), minor_breaks = FALSE) +
  labs(shape = "Residence During First 2 Years:", color = "Residence During First 2 Years:") +
  theme(legend.position = "bottom")

# Part F
fit1$coefficients[2]

# Part G
fit1$coefficients[3]
