amniotic <- data.frame(
  cells = c(
    1.13, 1.20, 1.00, 0.91, 1.05, 1.75, 1.45, 1.55, 1.64, 1.60, 2.30, 2.15, 2.25, 2.40, 2.49, 3.18, 3.10, 3.28, 3.35, 3.12
  ),
  temp = c(
    rep(40,5), rep(60,5), rep(80,5), rep(100,5)
  )
)

amniotic$ln_cells <- log(amniotic$cells)

# Part A
fit_a <- lm(ln_cells ~ temp, data = amniotic)

fit_a$coefficients
exp(fit_a$coefficients)
exp(confint(fit_a))

# log(cells_hat) = -0.6681674 + (0.0185546)X_temp

# For a unit increase in temperature, we would expect an average increase
# 0.018 in log cells.

# For temperature 0 the expected log cells is -0.67.

# Back transform the intercept and slope
exp(fit_a)
exp(confint(fit_a))

# For a unit increase in temperature, we would expect an average 
# of 1.9% increase in cells.

# For temperature 0 the geometric mean of expected cells is 0.51.

# Part B
par(mfrow = c(2, 2))

plot(x = amniotic$temp, y = amniotic$ln_cells, main = 'Scatterplot')
abline(fit_lm)

plot(x = amniotic$temp, y = rstudent(fit_a), main = "Jackknife Scatterplot")
abline(h = 0, lty = 2)

hist(x = rstudent(fit_a), freq = FALSE, main = "Jacknife Histogram")
curve(dnorm(x, mean = 0, sd = 1), add = TRUE)

plot(ppoints(length(rstudent(fit_a))), sort(pnorm(rstudent(fit_a))), main = "Normal P-P")
abline(a = 0, b = 1)

# Part C

