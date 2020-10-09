cmms <- data.frame(
  'cmms.score' = c('0-5', '6-10', '11-15', '16-20', '21-25', '26-30'),
  'non_demented' = c(0, 0, 3, 9, 16, 18),
  'demented' = c(2, 1, 4, 5, 3, 1),
  row.names = 1
)

cmms

# Part A
twenty_table <- data.frame(
  'demented' = c(sum(cmms[1:4, 2]), sum(cmms[5:6, 2])),
  'non_demented' = c(sum(cmms[1:4, 1]), sum(cmms[5:6, 1]))
)

rownames(twenty_table) <- c('below_20', 'above_20')
twenty_table

# Part B
sensitivity <- twenty_table[1, 1] / ( twenty_table[1, 1] + twenty_table[2, 1] )
sensitivity

specificity <- twenty_table[2, 2] / ( twenty_table[1, 2] + twenty_table[2, 2] )
specificity

# Part C
pvp <- twenty_table[1, 1] / ( twenty_table[1, 1] + twenty_table[1, 2] )
pvp

pvn <- twenty_table[2, 2] / ( twenty_table[2, 1] + twenty_table[2, 2] )
pvn

# Part D

# Part E
library(pROC)

cmms_roc <- matrix(data = NA, nrow = sum(cmms$non_demented) + sum(cmms$demented), ncol = dim(cmms)[2])
iter1 <- 1
iter2 <- 1

for (i in cmms$non_demented) {
  if (i > 0) {
    for (j in 1:i) {
      cmms_roc[iter1,] <- c(rownames(cmms[iter2,]), 1)
      iter1 <- iter1 + 1
    }
  }
  
  iter2 <- iter2 + 1
}

iter2 <- 1

for (i in cmms$demented) {
  if (i > 0) {
    for (j in 1:i) {
      cmms_roc[iter1,] <- c(rownames(cmms[iter2,]), 0)
      iter1 <- iter1 + 1
    }
  }
  
  iter2 <- iter2 + 1
}

cmms_roc

roc <- roc(cmms_roc[,1] ~ cmms_roc[,2])
plot(roc)

