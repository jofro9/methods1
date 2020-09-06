#' ---
#'   
#' Joseph Froelicher  
#' Dr. Alex Kaizer  
#' BIOS 6611  
#' 09/10/2020  
#' 
#' ---
#'

# Part A #
twos <- vector("double", 250)
iter <- 0


for ( i in seq(9001, 9250, 1) ) {
  iter = iter + 1
  twos[iter] <- i
}

set.seed(8675309)
rand <- sample(1:250, 30)
iter <- 0
sample <- vector("double", 30)

for (r in rand) {
  iter <- iter + 1
  sample[iter] <- twos[r]
}

# Part B #
setwd("C:/Users/jofro/OneDrive/cu/sem1/methods1/hw1")
names <- read.table("names.txt")
colnames(names) <- "name"
set.seed(8675309)
team_prob <- runif(30) # cut off of 0.67 for even teams
iter <- 0

for (t in team_prob) {
  iter <- iter + 1

  if (t <= 0.67) {
    names$team[iter] <- "red"

  } else {
    names$team[iter] <- "blue"
  }
}

head(names, 10)

table(names$team)

# Part C #
set.seed(8675309)
df <- data.frame("id" = 1:10, "age" =  runif(10, 20, 60))

older <- subset(df, age >= 45)
younger <- subset(df, age < 45)

print(older)
print(younger)

# Part D #
set.seed(8675309)
assignments <- data.frame(
  "id" = 1:100,
  "dietary_intervention" = runif(100, 0, 1),
  "pharma_intervention" = rbinom(100, 1, 0.3)
)

iter <- 0

for (a in assignments$dietary_intervention) {
  iter <- iter + 1

  if ( (a > 0.3) & sum(assignments$dietary_intervention == "ND") <= 70 ) {
    assignments$dietary_intervention[iter] = "ND"

  } else {
    assignments$dietary_intervention[iter] = "D"
  }
}

iter <- 0

for (a in assignments$pharma_intervention) {
  iter <- iter + 1

  if ( a == 1 & sum(assignments$pharma_intervention == "P") <= 30 ) {
    assignments$pharma_intervention[iter] = "P"

  } else {
    assignments$pharma_intervention[iter] = "NP"
  }
}

table(assignments$dietary_intervention, assignments$pharma_intervention)

# Part E #
hospital <- data.frame(
  "id" = c( rep.int(1, 40), rep.int(2, 40), rep.int(3, 40), rep.int(4, 40), rep.int(5, 40) ),
  "patient" = c(1:40, 1:40, 1:40, 1:40, 1:40),
  "improved" = rbinom(200, 1, 0.7)
)

improved1 <- 0
improved2 <- 0
improved3 <- 0
improved4 <- 0
improved5 <- 0
iter <- 0

for (h in hospital$improved) {
  iter <- iter + 1

  if ( h == 1 & (hospital$id[iter] == 1) ) {
    improved1 = improved1 + 1

  } else if ( h == 1 & (hospital$id[iter] == 2) ) {
    improved2 = improved2 + 1

  } else if ( h == 1 & (hospital$id[iter] == 3) ) {
    improved3 = improved3 + 1

  } else if ( h == 1 & (hospital$id[iter] == 4) ) {
    improved4 = improved4 + 1

  } else if ( h == 1 & (hospital$id[iter] == 5) ) {
    improved5 = improved5 + 1
  }
}

data.frame(
  "improvment" = c(
    improved1 / 40,
    improved2 / 40,
    improved3 / 40,
    improved4 / 40,
    improved5 / 40,
    sum(hospital$improved) / 200
  ),
  row.names = c(
    "hospital 1",
    "hospital 2",
    "hospital 3",
    "hospital 4",
    "hospital 5",
    "average"
  )
)
