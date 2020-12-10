library(DescTools)
library(dplyr)
library(furniture)
library(ggplot2)

blood = readRDS(file = "C:/Users/froelijo/dev/methods1/final/data/blood.rds")

# Mimmic Paper
kruskal.test(units ~ rbcagegroup, data = blood)

# Post Hoc Comparisons
comparisons = DunnTest(units ~ rbcagegroup, data = blood)
p.adjust(comparisons[[1]][,2], method = "bonferroni")

young_median = median(blood[blood$rbcagegroup == 1,]$units)
middle_median = median(blood[blood$rbcagegroup == 2,]$units)
old_median = median(blood[blood$rbcagegroup == 3,]$units)

# Examine the distribution of the data
par(mfrow = c(1, 3))

hist(blood[blood$rbcagegroup == 1,]$units)
hist(blood[blood$rbcagegroup == 2,]$units)
hist(blood[blood$rbcagegroup == 3,]$units)

kable(data.frame(cbind(comparisons[[1]], p)))

# Permutation Test
n = dim(blood)[1]
n1 = length(blood[blood$rbcagegroup == 1,])
n2 = length(blood[blood$rbcagegroup == 2,])
n3 = length(blood[blood$rbcagegroup == 3,])

b = 10000

result_mat = matrix(NA, nrow = b, ncol = 3)

set.seed(8675309)

for(i in 1:b) {
  group1 = sample(n, n1, replace = FALSE)
  group2 = sample(n, n2, replace = FALSE)
  group3 = sample(n, n3, replace = FALSE)
  
  result_mat[i, 1] = median(blood[group1,]$units)
  result_mat[i, 2] = median(blood[group2,]$units)
  result_mat[i, 3] = median(blood[group3,]$units)
}

blood_new = cbind(
  c(rep(1, length( result_mat[,1])), rep(2, length( result_mat[,2])), rep(3, length( result_mat[,1]))),
  c(result_mat[, 1], result_mat[, 2], result_mat[, 3])
)

colnames(blood_new) <- c("rbcagegroup", "units")

kruskal.test(units ~ rbcagegroup, blood_new)
comparisons_new = DunnTest(units ~ rbcagegroup, data = blood_new)
p.adjust(comparisons_new[[1]][,2], method = "bonferroni")

hist(log(blood_new[,2]))

# Table One
blood$rbcagegroup <- as.factor(blood$rbcagegroup)
levels(blood$rbcagegroup) = c(
  `1` = "Young",
  `2` = "Middle",
  `3` = "Old"
)

blood$aa <- as.factor(blood$aa)
levels(blood$aa) = c(
  `0` = "Not African American",
  `1` = "African American"
)

blood$famhx <- as.factor(blood$famhx)
levels(blood$famhx) = c(
  `0` = "No",
  `1` = "Yes"
)

table_one = table1(
  blood,
  factor(aa),
  factor(famhx),
  age,
  units,
  splitby = ~ rbcagegroup,
  var_names = c(
    aa = "Race",
    famhx = "Family history of disease",
    age = "Age",
    units = "Number of allogeneic units"
  ),
  FUN = function(x) paste0(round(median(x, na.rm=FALSE), 1), " (", round(sd(x, na.rm=FALSE), 1), ")"),
  digits = 1,
  total = TRUE
)

# Figure 1
ggplot(blood_tableone, aes(x = rbcagegroup, y = units, fill = factor(rbcagegroup))) +
  geom_boxplot() +
  ggtitle("Boxplot of Blood Age Groups, by Transfused Allogeneic RBC Units") +
  ylab("Number of allogeneic units") +
  xlab(NULL) +
  guides(fill = guide_legend(title = "RBC Age Group")) +
  geom_hline(yintercept = median(blood$units), linetype = "dashed", color = "orange", size = 1.5) +
  annotate("text", x = 3.3, y = 2.7, label = "Median = 2", color = "Orange", size = 4.5)
