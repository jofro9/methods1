setwd("C:/Users/froelijo/dev/methods1/hw10")
lead = read.csv('lead2.csv')

# Part A
fit1 = lm(iq ~ expose, data = lead)
fit1$coefficients[2]

fit2 = lm(iq ~ expose + resdur, data = lead)
fit2$coefficients[2]

fit3 = lm(resdur ~ expose, data = lead)
fit3$coefficients[2]
summary(fit3)

# Part B
(fit1$coefficients[2] - fit2$coefficients[2]) / fit1$coefficients[2] * 100

# Part C
se = sqrt(
  ( fit3$coefficients[2] ^ 2 ) *
  ( summary(fit2)$coefficients[3, 2] ^ 2 ) +
  ( fit2$coefficients[3] ^ 2 ) *
  ( summary(fit3)$coefficients[2, 2] ^ 2 )
)

z = ( fit3$coefficients[2] * summary(fit2)$coefficients[3, 1] ) / se
p = 2 * pnorm(z)

confint = c(
  ( fit3$coefficients[2] * summary(fit2)$coefficients[3, 1] ) - (qnorm(0.975) * se ),
  ( fit3$coefficients[2] * summary(fit2)$coefficients[3, 1] ) + (qnorm(0.975) * se )
)

# Part D
n <- dim(lead)[1]
b <- 10000

prop_vec = vector("double", b)
se_vec = vector("double", b)

set.seed(8675309)

for (i in 1:b) {
  new_resdur = sample(lead$resdur, n, replace = TRUE)
  new_expose = sample(lead$expose, n, replace = TRUE)
  new_iq = sample(lead$iq, n, replace = TRUE)

  new = data.frame(
    "resdur" = new_resdur,
    "expose" = new_expose,
    "iq" = new_iq
  )
  
  fit1 = lm(iq ~ expose, data = lead)
  fit2 = lm(iq ~ expose + resdur, data = new)
  fit3 = lm(resdur ~ expose, data = new)
  
  se_vec = sqrt(
    ( fit3$coefficients[2] ^ 2 ) *
    ( summary(fit2)$coefficients[3, 2] ^ 2 ) +
    ( fit2$coefficients[3] ^ 2 ) *
    ( summary(fit3)$coefficients[2, 2] ^ 2 )
  )
  
  prop_vec[i] = (fit1$coefficients[2] - fit2$coefficients[2]) / fit1$coefficients[2]
}

boot_confint = c(
  (mean(prop_vec) - (qnorm(0.975) * mean(se_vec))) * 100,
  (mean(prop_vec) + (qnorm(0.975) * mean(se_vec))) * 100
)
