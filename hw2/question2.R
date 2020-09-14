# Part A

N = 120
P = 0.01

binomial_prob <- pbinom(0.025, N, P)
poisson_prob <- ppois(0.025, N * P)

# Part B
library(plot.matrix)
library(ggplot2)
library(reshape2)

size_start <- 80
size_stop <- 400
size_step <- 40

prevalence_start <- 0.0025
prevalence_stop <- 0.025
prevalence_step <- 0.0025

PREVALENCE <- 0.025

difference.mat <- matrix(
  NA,
  nrow = prevalence_stop / prevalence_step,
  ncol = (size_stop - size_step) / size_step
)

i <- 0

for ( n in seq(size_start, size_stop, size_step) ) {
  i <- i + 1
  j <- 0
  
  for ( p in seq(prevalence_start, prevalence_stop, prevalence_step)) {
    j <- j + 1
    
    difference.mat[j, i] <- ppois(PREVALENCE, n * p) - pbinom(PREVALENCE, n, p) 
    
  }
}

difference.df <- data.frame(
  'pop_prev' = c('0.25%','0.5%','0.75%','1%','1.25%','1.5%','1.75%','2%','2.25%','2.5%'),
  difference.mat
)

colnames(difference.df) <- c('pop_prev','80','120','160','200','240','280','320','360','400')

difference.df <- melt(difference.df, id.vars="pop_prev", value.name="difference", variable.name="n")

ggplot(data=difference.df, aes(n, difference, group = pop_prev, colour = pop_prev)) +
  geom_line() +
  geom_point( size = 4, shape = 21, fill="white")

