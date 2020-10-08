# Exercise 1
dat1 <- read.csv('https://ucdenver.instructure.com/courses/446290/files/12032694/download')

# Part A
ar_knee <- dat1[dat1$ahrq_ccs == "Arthroplasty knee",c("complication", "hour")]

comp_table <- table(ar_knee$hour < 12, ar_knee$complication)
comp_df <- data.frame('before_12' = comp_table[,1], 'after_12' = comp_table[,2])
rownames(comp_df) <- c('no complication', 'complication')
comp_df

# Part B
rd <- comp_df[2, 2] / (comp_df[2, 2] + comp_df[1,2]) -  comp_df[2, 2] / (comp_df[2, 1] + comp_df[2,2])
rd
