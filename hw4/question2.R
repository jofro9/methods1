# Part A
# unknown standard deviation
A <- power.t.test(n=5, delta=100, sd=75, sig.level=0.05, power=NULL, type='one.sample')
A$power

# Part B
# unknown standard deviation
B <- power.t.test(n=NULL, delta=100, sd=75, sig.level=0.05, power=0.9, type='one.sample')
B$n

# Part C
# unknown standard deviation
C <- power.t.test(n=5, delta=NULL, sd=75, sig.level=0.05, power=0.9, type='one.sample')
C$delta

# Part D
# unknown standard deviation
D <- power.t.test(n=5, delta=NULL, sd=75, sig.level=0.05, power=0.8, type='one.sample')
D$delta
