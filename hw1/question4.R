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
naws <- read.csv("NAWS2014.csv", header = TRUE)

# Part B #
hist(
  naws$A09,
  main = "Histogram of years of school completed by migrant workers",
  xlab = "Number of years of school completed",
  ylab = "Number of workers"
)

# Part C #
mean(naws$A09)
median(naws$A09)

# https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode #
getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getMode(naws$A09)

# Part D #
naws["category_edu"] <- NA
iter <- 0

for (a in naws$A09) {
  iter <- iter + 1
  
  if (a <= 5) {
    naws$category_edu[iter] <- "00-05"
    
  } else if (a <= 8) {
    naws$category_edu[iter] <- "06-08"
    
  } else if (a <= 11) {
    naws$category_edu[iter] <- "09-11"
    
  } else {
    naws$category_edu[iter] <- "12+"
  }
}

head(naws$category_edu)

# Part E #
cat_table <- table(naws$category_edu) / length(naws$category_edu)
cat_table

# Part F #
mockdata <- data.frame(subject = 1:800, random = runif(n = 800))
head(mockdata)

mockdata["educ_cat"] <- NA
mockdata["educ_years"] <- NA
mockdata["educ_stop_yn"] <- NA

level1 <- cat_table[1]
level2 <- cat_table[1] + cat_table[2]
level3 <- cat_table[1] + cat_table[2] + cat_table[3]
iter <- 0

for (r in mockdata$random) {
  iter <- iter + 1
  
  if (r <= level1) {
    mockdata$educ_cat[iter] <- "00-05"
    mockdata$educ_years[iter] <- runif(1, 0, 6)
    mockdata$educ_stop_yn[iter] <- rbinom(1, 1, 0.8)
    
  } else if ( r > level1 & r <= level2 ) {
    mockdata$educ_cat[iter] <- "06-08"
    mockdata$educ_years[iter] <- runif(1, 6, 9)
    mockdata$educ_stop_yn[iter] <- rbinom(1, 1, 0.8)
    
  } else if ( r > level2 & r <= level3 ) {
    mockdata$educ_cat[iter] <- "09-11"
    mockdata$educ_years[iter] <- runif(1, 9, 12)
    mockdata$educ_stop_yn[iter] <- rbinom(1, 1, 0.8)
    
  } else {
    mockdata$educ_cat[iter] <- "12+"
    mockdata$educ_years[iter] <- runif(1, 12, 17)
    mockdata$educ_stop_yn[iter] <- rbinom(1, 1, 0.8)
    
  }
}

iter <- 0
for (c in mockdata$educ_cat) {
  iter <- iter + 1
  
  if ( c == "06-08" & mockdata$educ_stop_yn[iter] == 1 ) {
    mockdata$educ_years[iter] <- 6
  } else if ( c == "09-11" & mockdata$educ_stop_yn[iter] == 1 ) {
    mockdata$educ_years[iter] <- 9
  } else if ( c == "12+" & mockdata$educ_stop_yn[iter] == 1 ) {
    mockdata$educ_years[iter] <- 12
  }
}

mockdata$educ_years <- trunc(mockdata$educ_years)

# Part G #
hist(
  mockdata$educ_years,
  main = "Histogram of years of school completed by migrant workers (Random)",
  xlab = "Number of years of school completed",
  ylab = "Number of workers",
  breaks = 16
)
