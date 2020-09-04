# Joseph Froelicher
# Dr. Alex Kaizer
# BIOS 6611
# 09/10/2020

### Problem 1 ###

# Part A #
library(datasets)
library(dplyr)
sleep <- datasets::sleep
head(sleep)

sleep1 <- filter(sleep, sleep$group == "1")
sleep2 <- filter(sleep, sleep$group == "2")
t.test(sleep1$extra, sleep2$extra)

# Part B #
t.test(sleep1$extra, sleep2$extra, paired = TRUE)

# Part C #

