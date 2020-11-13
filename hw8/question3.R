library(car)

setwd("C:/Users/froelijo/dev/methods1/hw8")
fram <- read.csv("frmgham2_baseline_subset.csv")

# Part B
head(fram)
dim(fram)

fram <- fram[,c(3, 7, 9, 15, 17, 18, 19)]

for(column in 1:dim(fram)[2]) {
  fram <- fram[!is.na(fram[,column]),]
}

head(fram)
dim(fram)

# Part C
fit_full <- glm(TOTCHOL ~ CURSMOKE + BMI + PREVCHD + PREVMI + PREVSTRK + PREVHYP, fram)
fit_empty <- glm(TOTCHOL ~ 1, fram)
anova(fit_full, fit_empty)

# Part D
fit_nocardio <- glm(TOTCHOL ~ CURSMOKE + BMI, fram)
anova(fit_full, fit_nocardio)

# Part E
fit_nosmoke <- glm(TOTCHOL ~ BMI + PREVCHD + PREVMI + PREVSTRK + PREVHYP, fram)
anova(fit_full, fit_nosmoke)

# Part F
vif(fit_full)
