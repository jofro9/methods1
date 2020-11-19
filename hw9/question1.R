snp <- c(0.04, 0.1, 0.4, 0.55, 0.34, 0.62, 0.001, 0.010, 0.8, 0.005)
alpha <- 0.05

snp_bonferroni <- p.adjust(snp, method = "bonferroni")
snp_fdr <- p.adjust(snp, method = "fdr")

print("After a bonferroni correction for multiple comparisons:")
for(i in 1:length(snp_bonferroni)) {
  if(snp_bonferroni[i] <= alpha) {
    print(c("P-value", i , "was significant"), quote = FALSE)
  }
}

print("After a false discovery rate correction for multiple comparisons:")
for(i in 1:length(snp_fdr)) {
  if(snp_fdr[i] < alpha) {
    print(c("P-value ", i, " was significant."), quote = FALSE)
  }
}
