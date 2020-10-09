nausea <- data.frame(
  c('A', 'B'),
  "Yes" = c(65, 72),
  "No" = c(15, 48),
  row.names = 1
)

nausea

nausea_table <- as.table(cbind(c(65, 72), c(15, 48)))
nausea_table

# Part A

# by hand
alpha <- 0.05

# risk difference
rd <- (nausea[1, 1] / ( nausea[1, 1] + nausea[1, 2] )) - (nausea[2, 1] / ( nausea[2, 1] + nausea[2, 2]) )

rd_ci <- c(
  rd + (
    qnorm(1 - (alpha / 2)) * sqrt(
    (( nausea[1, 1] + nausea[2, 1] ) / ( nausea[1, 1] + nausea[2, 1] + nausea[1, 2] + nausea[2, 2] )) *
      ( 1 - ((nausea[1, 1] + nausea[2, 1]) / (nausea[1, 1] + nausea[2, 1] + nausea[1, 2] + nausea[2, 2])) ) *
      ( (nausea[1, 1] + nausea[1, 2])^-1 + (nausea[2, 1] + nausea[2, 2])^-1 )
    )
  ),
  exp(log(rd)) - (
    qnorm(1 - (alpha / 2)) * sqrt(
    (( nausea[1, 1] + nausea[2, 1] ) / ( nausea[1, 1] + nausea[2, 1] + nausea[1, 2] + nausea[2, 2] )) *
      ( 1 - ((nausea[1, 1] + nausea[2, 1]) / (nausea[1, 1] + nausea[2, 1] + nausea[1, 2] + nausea[2, 2])) ) *
      ( (nausea[1, 1] + nausea[1, 2])^-1 + (nausea[2, 1] + nausea[2, 2])^-1 )
    )
  )
)

# risk ratio
rr <- ( nausea[1, 1] / ( nausea[1, 1] + nausea[1, 2] ) ) / ( nausea[2, 1] / ( nausea[2, 1] + nausea[2, 2] ) )

rr_ci <- c(
  exp(
    log(rr) + (
      qnorm(1  - (alpha / 2)) * sqrt(
        nausea[1, 2] / ( nausea[1, 1] * ( nausea[1, 1] + nausea[1, 2] ) ) +
        nausea[2, 2] / ( nausea[2, 1] * ( nausea[2, 1] + nausea[2, 2] ) )
      )
    )
  ),
  exp(
    log(rr) - (
      qnorm(1 - (alpha / 2)) * sqrt(
        nausea[1, 2] / ( nausea[1, 1] * ( nausea[1, 1] + nausea[1, 2] ) ) +
        nausea[2, 2] / ( nausea[2, 1] * ( nausea[2, 1] + nausea[2, 2] ) )
      )
    )
  )
)

# odds ratio
or <- ( nausea[1, 1] * nausea[2, 2] ) / ( nausea[2, 1] * nausea[1, 2] )

or_ci <- c(
  exp(
    log(or) + (
      qnorm(1 - (alpha / 2)) * sqrt(nausea[1, 1]^-1 + nausea[1, 2]^-1 + nausea[2, 1]^-1 + nausea[2, 2]^-1)
    )
  ),
  exp(log(or) - (
      qnorm(1 - (alpha / 2)) * sqrt(nausea[1, 1]^-1 + nausea[1, 2]^-1 + nausea[2, 1]^-1 + nausea[2, 2]^-1)
    )
  )
)


data.frame(
  'statistic' = c('rd', 'rr', 'or'),
  'point_estimate' = c(rd * 100, rr, or),
  'lower' = c(rd_ci[2] * 100, rr_ci[2], or_ci[2]),
  'upper' = c(rd_ci[1] * 100, rr_ci[1], or_ci[1])
)

# using epiR

library(epiR)

epi.2by2(nausea_table)

# Part B
chisq <- chisq.test(nausea_table, correct = FALSE)
chisq_correct <- chisq.test(nausea_table, correct = TRUE)
fisher <- fisher.test(nausea_table)
mcnemar <- mcnemar.test(nausea_table)

df <- data.frame(
  'approach' = c('chisq.test', 'chisq.test.corrected', 'fisher.test', 'mcnemar.nest'),
  'test_statistic' = c(chisq$statistic, chisq_correct$statistic, NA, mcnemar$statistic),
  'p_value' = round(c(chisq$p.value, chisq_correct$p.value, fisher$p.value, mcnemar$p.value), digits = 3)
)

iter <- 1
for (i in df$p_value) {
  if (i < 0.001) {
    df$p_value[iter] <- '<0.001'
  }
  
  iter <- iter + 1
}

df
