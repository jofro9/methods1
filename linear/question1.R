# Part A
A = matrix(c(3, 4, 2, 2), nrow = 2, ncol = 2, byrow = TRUE)
B = matrix(c(1, 3, 2, 4), nrow = 2, ncol = 2, byrow = TRUE)

dim(A)
dim(B)

# Part B
A[1, 2]

# Part C
A %+% B

# Part D
A - B

# Part E
A %*% B

# Part F
B %*% A

# Part G

# Part H
t(A)

# Part I
t(A %*% B)
t(B) %*% t(A)

# Part J
AI = solve(A)

# Part K
A %*% AI

