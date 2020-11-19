library(DescTools)

lung <- data.frame(
  group=c(
    rep('A',5),
    rep('B',12),
    rep('C',5)
  ),
  react=c(
    20.8,4.1,30,24.7,13.8,7.5,7.5,11.9,4.5,3.1,8,
    4.7,28.1,10.3,10,5.1,2.2,9.2,2,2.5,6.1,7.5
  )
)

# Part A
pairwise.t.test(x = lung$react, g = lung$group, pooled.sd = TRUE)

# Part B
aov <- aov(react ~ group, data = lung)
PostHocTest(aov, method = "hsd")

# Part C

